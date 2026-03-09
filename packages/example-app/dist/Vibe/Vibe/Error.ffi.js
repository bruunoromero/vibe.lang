export const create = (value) => {
  // Already an Error
  if (value instanceof Error) return value;

  // Primitive types
  if (typeof value === "string") return new Error(value);
  if (typeof value === "number" || typeof value === "boolean") {
    return new Error(String(value));
  }

  // Null / undefined
  if (value === null) return new Error("null was thrown");
  if (value === undefined) return new Error("undefined was thrown");

  // Object with a message property (e.g. axios errors, custom thrown objects)
  if (typeof value === "object") {
    if ("message" in value && typeof value.message === "string") {
      const err = new Error(value.message);
      if ("stack" in value && typeof value.stack === "string") {
        err.stack = value.stack;
      }
      if ("name" in value && typeof value.name === "string") {
        err.name = value.name;
      }
      return err;
    }

    // Fallback: serialize the object
    try {
      return new Error(JSON.stringify(value));
    } catch {
      return new Error("[unserializable object]");
    }
  }

  // Symbols, functions, or anything else
  return new Error(String(value));
};
