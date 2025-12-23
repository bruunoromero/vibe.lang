export type LexSource =
  | string
  | AsyncIterable<string | ArrayBufferView | ArrayBuffer>
  | ReadableStream<string | ArrayBufferView | ArrayBuffer>
  | { stream: () => ReadableStream<string | ArrayBufferView | ArrayBuffer> };

type NormalizedIterable = AsyncIterable<string>;

type Chunk = string | ArrayBufferView | ArrayBuffer;

const isAsyncIterable = (value: unknown): value is AsyncIterable<Chunk> =>
  value !== null &&
  typeof value === "object" &&
  Symbol.asyncIterator in (value as Record<string, unknown>);

const isReadableStream = (value: unknown): value is ReadableStream<Chunk> =>
  value !== null &&
  typeof value === "object" &&
  typeof (value as ReadableStream<Chunk>).getReader === "function";

const hasStreamMethod = (
  value: unknown
): value is { stream: () => ReadableStream<Chunk> } =>
  value !== null &&
  typeof value === "object" &&
  typeof (value as { stream?: unknown }).stream === "function";

const fromReadableStream = (
  readable: ReadableStream<Chunk>
): AsyncIterable<Chunk> => ({
  async *[Symbol.asyncIterator]() {
    const reader = readable.getReader();
    try {
      while (true) {
        const { value, done } = await reader.read();
        if (done) {
          break;
        }
        if (value !== undefined) {
          yield value;
        }
      }
    } finally {
      reader.releaseLock();
    }
  },
});

const normalizeIterable = (
  iterable: AsyncIterable<Chunk>
): NormalizedIterable => ({
  async *[Symbol.asyncIterator]() {
    const decoder = new TextDecoder();
    for await (const chunk of iterable) {
      if (chunk === undefined || chunk === null) {
        continue;
      }
      if (typeof chunk === "string") {
        if (chunk.length > 0) {
          yield chunk;
        }
        continue;
      }
      if (chunk instanceof ArrayBuffer) {
        const text = decoder.decode(new Uint8Array(chunk), { stream: true });
        if (text.length > 0) {
          yield text;
        }
        continue;
      }
      if (ArrayBuffer.isView(chunk)) {
        const view = chunk as ArrayBufferView;
        const text = decoder.decode(
          new Uint8Array(view.buffer, view.byteOffset, view.byteLength),
          { stream: true }
        );
        if (text.length > 0) {
          yield text;
        }
        continue;
      }
      const text = String(chunk);
      if (text.length > 0) {
        yield text;
      }
    }
    const trailing = decoder.decode();
    if (trailing.length > 0) {
      yield trailing;
    }
  },
});

const stringIterable = (value: string): NormalizedIterable => ({
  async *[Symbol.asyncIterator]() {
    yield value;
  },
});

const toIterable = (source: LexSource): NormalizedIterable => {
  if (typeof source === "string") {
    return stringIterable(source);
  }
  if (isAsyncIterable(source)) {
    return normalizeIterable(source);
  }
  if (isReadableStream(source)) {
    return normalizeIterable(fromReadableStream(source));
  }
  if (hasStreamMethod(source)) {
    return normalizeIterable(fromReadableStream(source.stream()));
  }
  throw new Error("Unsupported lexer source type");
};

export class CharacterStream {
  private readonly iterator: AsyncIterator<string>;
  private buffer = "";
  private done = false;

  constructor(source: LexSource) {
    this.iterator = toIterable(source)[Symbol.asyncIterator]();
  }

  private async fill(min: number): Promise<void> {
    while (!this.done && this.buffer.length < min) {
      const { value, done } = await this.iterator.next();
      if (done) {
        this.done = true;
        break;
      }
      if (value) {
        this.buffer += value;
      }
    }
  }

  async peek(offset = 0): Promise<string> {
    await this.fill(offset + 1);
    return this.buffer[offset] ?? "";
  }

  async advance(): Promise<string> {
    await this.fill(1);
    if (this.buffer.length === 0) {
      this.done = true;
      return "";
    }
    const ch = this.buffer[0] ?? "";
    this.buffer = this.buffer.slice(1);
    return ch;
  }
}
