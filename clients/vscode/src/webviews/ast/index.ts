import { mount } from "svelte";
import App from "./App.svelte";

const vscode = acquireVsCodeApi();

mount(App, {
  target: document.getElementById("app")!,
  context: new Map([["vscode", vscode]]),
});
