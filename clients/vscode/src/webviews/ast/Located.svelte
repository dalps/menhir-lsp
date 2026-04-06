<script lang="ts">
  import Located from "./Located.svelte";
  import type { Range, Position } from "vscode-languageclient";
  import Terminal from "./Terminal.svelte";
  import { slide } from "svelte/transition";
  import { getContext } from "svelte";

  import { type VsCode, type ASTNode, type Offset, isInRange } from "./types";
  import Array from "./Array.svelte";
  import { on } from "svelte/events";

  const vscode = getContext<VsCode>("vscode");

  interface Props extends ASTNode {
    expanded?: boolean;
    expandParent: () => void;
  }

  let {
    expanded = $bindable(false),
    range,
    rawRange,
    value,
    expandParent,
  }: Props = $props();

  on(window, "message", (event) => {
    switch (event.data.type) {
      case "focus":
        reveal(event.data.offset);
        break;
    }
  });

  function toggle() {
    expanded = !expanded;
    // expandParent();
  }

  function reveal(offset: Offset) {
    if (isInRange(offset, rawRange)) {
      expanded = true;
    }
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

{#snippet renderNode(node: ASTNode)}
  <Located expandParent={toggle} {...node} />
{/snippet}

<button class="range" onclick={toggle} onpointerover={requestHighlight}>
  {expanded ? "-" : "+"}
  {@render showRange(range)}
</button>

{#if expanded}
  {#if typeof value === "string"}
    <Terminal {value} />
  {:else}
    <Array elements={value} renderElement={renderNode} />
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
