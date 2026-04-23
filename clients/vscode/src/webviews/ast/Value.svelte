<script lang="ts">
  import Array from "./Array.svelte";
  import Located from "./Located.svelte";
  import Object from "./Object.svelte";
  import Terminal from "./Terminal.svelte";

  let { value }: { value: any } = $props();
</script>

{#if typeof value === "string"}
  <Terminal {value} />
{:else if value.length >= 0}
  <Array expanded elements={value} />
{:else if value.range !== undefined}
  <Located {...value} />
{:else}
  <Object fields={value} />
{/if}
