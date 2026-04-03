<script lang="ts">
  import Located from "./Located.svelte";
  import type { Range } from "vscode-languageclient";
  import Terminal from "./Terminal.svelte";

  interface Props {
    expanded: boolean;
    range: Range;
    value: Props[] | string;
  }

  const { expanded = $bindable(false), range, value }: Props = $props();
</script>

{#if typeof value === "string"}
  <Terminal {value} />
{:else}
  {#each value as node}
    <Located {...node} />
  {/each}
{/if}
