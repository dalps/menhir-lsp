<script lang="ts">
  import { getContext } from "svelte";
  import type { Position, Range } from "vscode-languageclient";

  import { on } from "svelte/events";
  import { type ASTNode, type Offset, type VsCode, isInRange } from "./types";
  import Value from "./Value.svelte";
  import Terminal from "./Terminal.svelte";

  const vscode = getContext<VsCode>("vscode");

  interface Props extends ASTNode {
    expanded?: boolean;
    highlighted?: boolean;
    expandParent: () => void;
  }

  let {
    expanded = $bindable(false),
    highlighted = $bindable(false),
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
      expanded = highlighted = true;
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
  <span class="position">{line + 1}:{character + 1}</span>
{/snippet}

{#snippet showRange({ start, end }: Range)}
  <span class="range"
    >[{@render showPosition(start)}-{@render showPosition(end)}]</span
  >
{/snippet}

<span class={{ highlighted }}>
  <button class="range" onclick={toggle} onpointerover={requestHighlight}>
    {expanded ? "-" : "+"}
    <span class="switch">
      {#if value.type}
        <Terminal value={value.type} />
      {/if}
      {@render showRange(range)}
    </span>
  </button>

  {#if expanded}
    <Value value={value.type ? value.value : value} />
  {/if}
</span>

<style>
  button.range {
    appearance: none;
    outline: none;
    border: none;
    border-radius: 2px;

    background-color: var(--range-button-bg);
    color: var(--range-button-text);

    :hover {
      text-decoration: underline;
    }

    &:hover {
      background-color: var(--range-button-bg-hover);
    }
  }

  .highlighted {
    background-color: rgba(140, 255, 0, 0.1);
  }
</style>
