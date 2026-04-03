<script lang="ts">
  import { getContext } from "svelte";
  import { on } from "svelte/events";
  import { createSubscriber } from "svelte/reactivity";
  import type { WebviewApi } from "vscode-webview";
  import Located from "./Located.svelte";

  type ASTNode = { range: any; value: ASTNode[] | string };

  const vscode = getContext("vscode") as WebviewApi<unknown>;

  let ast = $state<ASTNode[]>([]);

  on(window, "message", (event) => {
    switch (event.data.type) {
      case "publishAst":
        ast = event.data.data;
        break;
    
      default:
        break;
    }
  });
</script>

<h1>Tree Browser</h1>
<div id="tree">
  {#each ast as node}
    <Located expanded {...node} />
  {/each}
</div>
