<script lang="ts">
  // MarkCraft: Interactive code playground component
  import { onMount, createEventDispatcher } from "svelte";
  import { writable, derived } from "svelte/store";
  import { fade, slide } from "svelte/transition";

  export let initialCode: string = "";
  export let language: string = "javascript";

  const dispatch = createEventDispatcher();
  const code = writable(initialCode);
  const output = writable<string[]>([]);
  const isRunning = writable(false);
  const theme = writable<"dark" | "light">("dark");

  const lineCount = derived(code, ($code) =>
    $code.split("\n").length
  );

  const charCount = derived(code, ($code) => $code.length);

  let editor: HTMLTextAreaElement;

  onMount(() => {
    editor?.focus();
    return () => output.set([]);
  });

  async function runCode() {
    $isRunning = true;
    $output = [];
    try {
      const originalLog = console.log;
      console.log = (...args) => {
        output.update((o) => [...o, args.join(" ")]);
      };
      const fn = new Function($code);
      await fn();
      console.log = originalLog;
      dispatch("run", { success: true });
    } catch (err: unknown) {
      const msg = err instanceof Error ? err.message : String(err);
      output.update((o) => [...o, `Error: ${msg}`]);
      dispatch("run", { success: false, error: msg });
    } finally {
      $isRunning = false;
    }
  }

  function handleKeydown(e: KeyboardEvent) {
    if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
      e.preventDefault();
      runCode();
    }
    if (e.key === "Tab") {
      e.preventDefault();
      const start = editor.selectionStart;
      $code = $code.slice(0, start) + "  " + $code.slice(editor.selectionEnd);
      requestAnimationFrame(() => {
        editor.selectionStart = editor.selectionEnd = start + 2;
      });
    }
  }
</script>

<div class="playground" class:dark={$theme === "dark"}>
  <header>
    <h3>MarkCraft Playground</h3>
    <div class="controls">
      <span class="stats">{$lineCount} lines &middot; {$charCount} chars</span>
      <button on:click={() => ($theme = $theme === "dark" ? "light" : "dark")}>
        {$theme === "dark" ? "Light" : "Dark"}
      </button>
      <button class="run" on:click={runCode} disabled={$isRunning}>
        {$isRunning ? "Running..." : "Run (Cmd+Enter)"}
      </button>
    </div>
  </header>
  <div class="editor-area">
    <div class="gutter">
      {#each Array($lineCount) as _, i}
        <span>{i + 1}</span>
      {/each}
    </div>
    <textarea bind:this={editor} bind:value={$code}
      on:keydown={handleKeydown} spellcheck="false" />
  </div>
  {#if $output.length > 0}
    <div class="output" transition:slide>
      {#each $output as line, i (i)}
        <pre in:fade={{ delay: i * 30 }}>{line}</pre>
      {/each}
    </div>
  {/if}
</div>

<style>
  .playground { border: 1px solid var(--border); border-radius: 12px; overflow: hidden; }
  .dark { --bg: #0a0a12; --fg: #e8e6f0; --border: #2a2a3e; }
  header { display: flex; justify-content: space-between; padding: 12px 16px; }
  .editor-area { display: flex; }
  .gutter { width: 40px; text-align: right; padding: 8px; opacity: 0.4; }
  textarea { flex: 1; min-height: 200px; resize: vertical; font-family: monospace; }
  .output { border-top: 1px solid var(--border); padding: 12px; }
  .run { background: #6b54d1; color: white; border-radius: 6px; }
</style>
