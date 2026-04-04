<script lang="ts">
  import { getContext } from "svelte";
  import { on } from "svelte/events";
  import "./app.css";
  import Located from "./Located.svelte";
  import type { ASTNode, VsCode } from "./types";

  const vscode = getContext<VsCode>("vscode");

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
  <ul>
    {#each ast as node}
      <li>
        <Located expanded {...node} />
      </li>
    {/each}
  </ul>
</div>
