<script lang="ts">
  import Terminal from "./Terminal.svelte";
  import Value from "./Value.svelte";

  let { type, value } = $props();
</script>

<Terminal value={type} />
<Value {value} />
