import * as vscode from "vscode";
import { Position, Range, Uri } from "vscode";
import * as assert from "assert";
import { activate, getDocUri } from "./helper";

/**
 * Run with: Ctrl+Shift+D > Menhir LSP - E2E Tests
 */
suite("should auto-complete symbols in a parser", () => {
  const uri = getDocUri("grammar6.mly");

  test("completes a token symbol", async () => {
    await testCompletion(uri, new Position(0, 0), {
      items: [
        { label: "TOKEN_A", kind: vscode.CompletionItemKind.Value },
        { label: "TOKEN_B", kind: vscode.CompletionItemKind.Value },
        { label: "Tc", kind: vscode.CompletionItemKind.Value },
      ],
    });
  });

  test("completes start rule's type parameter", async () => {
    await testCompletion(uri, new Position(4, 9), {
      items: [{ label: "unit", kind: vscode.CompletionItemKind.TypeParameter }],
    });
  });

  test("completes a semantic action", async () => {
    await testCompletion(uri, new Position(9, 21), {
      items: [{ label: "Lexing", kind: vscode.CompletionItemKind.Module }],
    });
  });
});

async function testCompletion(
  uri: Uri,
  pos: Position,
  expectedCompletions: vscode.CompletionList,
) {
  await activate(uri);

  const actualCompletionList = (await vscode.commands.executeCommand(
    "vscode.executeCompletionItemProvider",
    uri,
    pos,
  )) as vscode.CompletionList;

  assert.ok(actualCompletionList.items.length >= 2);

  expectedCompletions.items.forEach((expected, i) => {
    const actual = actualCompletionList.items.find(
      (actual) =>
        actual.label === expected.label && actual.kind === expected.kind,
    );

    assert.ok(actual, `"${expected.label}" not found in completions.`);
  });
}
