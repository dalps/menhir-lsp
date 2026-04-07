import { mount } from "svelte";
import App from "./App.svelte";
import { ASTNode } from "./types";

const vscode = acquireVsCodeApi();

const ast = await new Promise<ASTNode[]>((resolve, reject) => {
  window.addEventListener("message", (event) => {
    resolve(event.data.data);
  });
});

mount(App, {
  target: document.getElementById("app")!,
  props: { ast },
  context: new Map([["vscode", vscode]]),
});
