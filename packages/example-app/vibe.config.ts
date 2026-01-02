import { defineConfig } from "@vibe/config";

export default defineConfig({
  package: {
    sources: ["./src"],
    outDir: "./dist",
    entry: "./src/main.lang",
  },
  formatter: {},
});
