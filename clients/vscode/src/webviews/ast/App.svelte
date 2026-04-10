<script lang="ts">
  import { getContext } from "svelte";
  import { on } from "svelte/events";
  import "./app.css";
  import type { ASTNode, VsCode } from "./types";
  import Value from "./Value.svelte";

  const vscode = getContext<VsCode>("vscode");

  let { ast } = $props();

  on(window, "message", (event) => {
    switch (event.data.type) {
      case "publishAst":
        ast = event.data.data;
        console.log("[webview] received new ast", ast);
        break;
    }
  });
</script>

<div id="tree">
  <Value value={$state.eager(ast)} />
</div>
