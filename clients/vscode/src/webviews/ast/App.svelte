<script lang="ts">
  import { getContext } from "svelte";
  import type { WebviewApi } from "vscode-webview";

  const { data } = $props();

  const vscode = getContext("vscode") as WebviewApi<unknown>;
</script>

<div id="tree">
  <h1>Tree here</h1>
</div>
