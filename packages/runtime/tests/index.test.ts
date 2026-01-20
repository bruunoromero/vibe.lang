import { describe, it, expect } from "bun:test";

describe.skip("runtime", () => {
  it("works", () => {
    expect(1 + 1).toBe(2);
  });
});
