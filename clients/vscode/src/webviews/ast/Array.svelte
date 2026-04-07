<script lang="ts">
  import Value from "./Value.svelte";

  interface Props {
    elements: any[];
  }

  let { elements }: Props = $props();
</script>

{#if elements.length === 0}
  <span class="delimiter">[]</span>
{:else}
  <span class="delimiter">[</span>
  <ul>
    {#each elements as value}
      <li>
        <Value {value} />
      </li>
    {/each}
  </ul>
  <span class="delimiter">]</span>
{/if}

<style>
  ul {
    list-style: none;
    padding-left: 1em;
  }
</style>
