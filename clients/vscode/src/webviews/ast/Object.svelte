<script lang="ts">
  import Value from "./Value.svelte";

  interface Props {
    fields: Record<string, any>;
  }

  let { fields }: Props = $props();

  let entries = $derived([...Object.entries(fields)]);
</script>

{#if entries.length === 0}
  <span class="delimiter">{"{}"}</span>
{:else}
  <span class="delimiter">{"{"}</span>
  <ul>
    {#each entries as [k, v]}
      {console.log("entry:", k)}
      <div>
        <span class="key">{k}</span>:
        <Value value={v} />
      </div>
    {/each}
  </ul>
  <span class="delimiter">{"}"}</span>
{/if}

<style>
  ul {
    list-style: none;
    padding-left: 1em;
  }

  .key {
    color: orange;
  }
</style>
