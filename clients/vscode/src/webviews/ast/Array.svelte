<script lang="ts">
  import { enPluralRules } from "./types";
  import Value from "./Value.svelte";

  interface Props {
    expanded?: boolean;
    elements: any[];
  }

  let { expanded = $bindable(true), elements }: Props = $props();

  function toggle() {
    expanded = !expanded;
  }
</script>

{#if elements.length === 0}
  <span class="delimiter">{"[]"}</span>
{:else if expanded}
  <button class="delimiter" onclick={toggle}>{"["}</button>
  <ul>
    {#each elements as value}
      <li>
        <Value {value} />
      </li>
    {/each}
  </ul>
  <button class="delimiter" onclick={toggle}>{"]"}</button>
{:else}
  <button class="delimiter" onclick={toggle}
    >{"["}<em
      >{`${elements.length} ${enPluralRules.select(elements.length) === "one" ? "item" : "items"}`}</em
    >{"]"}</button
  >
{/if}

<style>
  ul {
    list-style: none;
    padding-left: 1em;
  }

  li {
    padding-top: 0.2rem;
  }

  button.delimiter:hover {
    text-decoration: underline;
  }
</style>
