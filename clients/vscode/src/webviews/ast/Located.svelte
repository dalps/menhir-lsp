<script lang="ts">
  import Located from "./Located.svelte";
  import type { Range, Position } from "vscode-languageclient";
  import Terminal from "./Terminal.svelte";
  import { slide } from "svelte/transition";
  import { getContext } from "svelte";

  import type { VsCode, ASTNode } from "./types";

  const vscode = getContext<VsCode>("vscode");

  type Props = ASTNode;

  let { expanded = $bindable(false), range, value }: Props = $props();

  function toggle() {
    expanded = !expanded;
  }

  function requestHighlight() {
    let { start, end } = range;

    vscode.postMessage({
      command: "highlight",
      data: {
        // Proxy objects are not serializable so we have to do this crap
        range: {
          start: { ...start },
          end: { ...end },
        },
      },
    });
  }
</script>

{#snippet showPosition({ character, line }: Position)}
  <span class="position">{line}:{character}</span>
{/snippet}

{#snippet showRange({ start, end }: Range)}
  <span class="range"
    >[{@render showPosition(start)}-{@render showPosition(end)}]</span
  >
{/snippet}

<button class="range" onclick={toggle} onpointerover={requestHighlight}>
  {expanded ? "-" : "+"}
  {@render showRange(range)}
</button>

{#if expanded}
  {#if typeof value === "string"}
    <Terminal {value} />
  {:else}
    <ul transition:slide={{ duration: 300 }}>
      {#each value as node}
        <li>
          <Located {...node} />
        </li>
      {/each}
    </ul>
  {/if}
{/if}

<style>
button.range {
  appearance: none;
  outline: none;

  :hover {
    text-decoration: underline;
  }
}
</style>
