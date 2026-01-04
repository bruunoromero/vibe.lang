const isAsyncIterable = (value) => value !== null &&
    typeof value === "object" &&
    Symbol.asyncIterator in value;
const isReadableStream = (value) => value !== null &&
    typeof value === "object" &&
    typeof value.getReader === "function";
const hasStreamMethod = (value) => value !== null &&
    typeof value === "object" &&
    typeof value.stream === "function";
const fromReadableStream = (readable) => ({
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
        }
        finally {
            reader.releaseLock();
        }
    },
});
const normalizeIterable = (iterable) => ({
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
                const view = chunk;
                const text = decoder.decode(new Uint8Array(view.buffer, view.byteOffset, view.byteLength), { stream: true });
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
const stringIterable = (value) => ({
    async *[Symbol.asyncIterator]() {
        yield value;
    },
});
const toIterable = (source) => {
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
    iterator;
    buffer = "";
    done = false;
    constructor(source) {
        this.iterator = toIterable(source)[Symbol.asyncIterator]();
    }
    async fill(min) {
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
    async peek(offset = 0) {
        await this.fill(offset + 1);
        return this.buffer[offset] ?? "";
    }
    async advance() {
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
//# sourceMappingURL=stream.js.map