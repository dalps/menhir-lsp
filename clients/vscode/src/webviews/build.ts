// This script is responsible for building the webviews of the extension.

import esbuild from "esbuild";
import sveltePlugin from "esbuild-svelte";

esbuild.build({
  entryPoints: ["./src/webviews/ast/index.ts", "./src/webviews/ast/index.html"],
  bundle: true,
  outdir: "./out/webviews/ast",
  loader: { ".html": "copy" },
  sourcemap: "inline",
  format: "esm",
  plugins: [sveltePlugin()],
});
