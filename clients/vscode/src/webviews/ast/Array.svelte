<script lang="ts">
  interface Props {
    elements: any[];
    renderElement;
  }

  let { elements, renderElement }: Props = $props();
</script>

{#if elements.length === 0}
  <p>[]</p>
{:else}
  <p>[</p>
  <ul>
    {#each elements as e}
      <li>
        {@render renderElement(e)}
      </li>
    {/each}
  </ul>
  <p>]</p>
{/if}

<style>
  ul {
    list-style: none;
    padding-left: 1em;
  }
</style>
