import { defineConfig } from "@vibe/config";

export default defineConfig({
  package: {
    sources: ["./src"],
    outDir: "./src",
    entry: "./src/prelude.lang",
  },
});
