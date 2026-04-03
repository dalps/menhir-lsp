<script lang="ts">
  import Located from "./Located.svelte";
  import type { Range, Position } from "vscode-languageclient";
  import Terminal from "./Terminal.svelte";

  interface Props {
    expanded: boolean;
    range: Range;
    value: Props[] | string;
  }

  const { expanded = $bindable(false), range, value }: Props = $props();
</script>

{#snippet showRange({ start, end }: Range)}
  <span class="range"
    >[{start.line}:{start.character}-{end.line}:{end.character}]</span
  >
{/snippet}

{@render showRange(range)}
{#if typeof value === "string"}
  <Terminal {value} />
{:else}
  <ul>
    {#each value as node}
      <li>
        <Located {...node} />
      </li>
    {/each}
  </ul>
{/if}

<style>
  :global {
    body {
      font-family: "Courier New", Courier, monospace;
    }
  }
</style>
