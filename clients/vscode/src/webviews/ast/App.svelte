<script lang="ts">
  import { getContext } from "svelte";
  import { on } from "svelte/events";
  import "./reset.css";
  import "./app.css";
  import Located from "./Located.svelte";
  import type { ASTNode, VsCode } from "./types";
  import Array from "./Array.svelte";

  const vscode = getContext<VsCode>("vscode");

  let ast = $state<ASTNode[]>([]);

  on(window, "message", (event) => {
    switch (event.data.type) {
      case "publishAst":
        ast = event.data.data;
        break;
    }
  });
</script>

{#snippet renderNode(node: ASTNode)}
  <Located expanded expandParent={() => {}} {...node} />
{/snippet}

<div id="tree">
  <Array elements={ast} renderElement={renderNode} />
</div>
