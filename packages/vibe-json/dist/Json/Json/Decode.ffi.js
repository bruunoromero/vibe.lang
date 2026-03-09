// JSON FFI - Low-level helpers for Json.Decode
// The opaque `JSON` type in Vibe is a raw JS value at runtime.

export const jsonParse = (ok, err, str) => {
  try {
    return ok(JSON.parse(str));
  } catch (e) {
    return err(e.message);
  }
};

// Type predicates
export const jsonIsNull = (v) => v === null;
export const jsonIsBool = (v) => typeof v === "boolean";
export const jsonIsInt = (v) => typeof v === "number" && Number.isInteger(v);
export const jsonIsNumber = (v) => typeof v === "number";
export const jsonIsString = (v) => typeof v === "string";
export const jsonIsArray = (v) => Array.isArray(v);
export const jsonIsObject = (v) =>
  v !== null && typeof v === "object" && !Array.isArray(v);

// Primitive accessors (caller must guard with predicates first)
export const jsonToBool = (v) => v;
export const jsonToInt = (v) => v;
export const jsonToFloat = (v) => v;
export const jsonToString = (v) => v;

// Array accessors
export const jsonArrayLength = (v) => v.length;
export const jsonArrayGet = (just, nothing, i, v) => {
  if (i >= 0 && i < v.length) {
    return just(v[i]);
  }
  return nothing;
};

// Object accessors
export const jsonObjectKeys = (v) => Object.keys(v);
export const jsonObjectGet = (just, nothing, key, v) => {
  if (Object.prototype.hasOwnProperty.call(v, key)) {
    return just(v[key]);
  }
  return nothing;
};
