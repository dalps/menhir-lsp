import * as vscode from "vscode";
import { Position, Range, Uri, CompletionItemKind } from "vscode";
import * as assert from "assert";
import { activate, getDocUri, P } from "./helper";

/**
 * Run with: Ctrl+Shift+D > Menhir LSP - E2E Tests
 */
suite("should auto-complete symbols in a parser", () => {
  const uri = getDocUri("grammar6.mly");

  test("completes a token symbol", async () => {
    await testCompletion(uri, P(0, 0), [
      { label: "TOKEN_A", kind: CompletionItemKind.Value },
      { label: "TOKEN_B", kind: CompletionItemKind.Value },
      { label: "Tc", kind: CompletionItemKind.Value },
    ]);
  });

  test("completes start rule's type parameter", async () => {
    await testCompletion(uri, P(4, 9), [
      { label: "unit", kind: CompletionItemKind.TypeParameter },
    ]);
  });

  test("completes a semantic action", async () => {
    await testCompletion(uri, P(9, 21), [
      { label: "Lexing", kind: CompletionItemKind.Module },
    ]);
  });
});

suite("should auto-complete symbols in a lexer", () => {
  const uri = getDocUri("calc_lexer.mly");

  test("suggests lexically-scoped regexp names inside new regexp definition", async () => {
    await testCompletion(
      uri,
      P(6, 21),
      [],
      [{ label: "bar" }, { label: "foo" }],
    );
    await testCompletion(uri, P(8, 12), [{ label: "bar" }]);
  });

  test("does suggest named regexp in case's regexp", async () => {});

  test("does suggest semantic variables bound by a referenced regexp in action", async () => {
    await testCompletion(
      uri,
      P(13, 23),
      [{ label: "ending" }, { label: "letter" }],
      [{ label: "code" }],
    );

    await testCompletion(uri, P(16, 14), [
      { label: "ending" },
      { label: "code" },
      { label: "letter" },
    ]);

    await testCompletion(
      uri,
      P(20, 24),
      [{ label: "line" }],
      [{ label: "code" }, { label: "letter" }, { label: "ending" }],
    );
  });
});

async function testCompletion(
  uri: Uri,
  pos: Position,
  includes: vscode.CompletionItem[],
  excludes?: vscode.CompletionItem[],
) {
  await activate(uri);

  const actualCompletionList = (await vscode.commands.executeCommand(
    "vscode.executeCompletionItemProvider",
    uri,
    pos,
  )) as vscode.CompletionList;

  assert.ok(actualCompletionList.items.length >= 2);

  includes.forEach((expected, i) => {
    const actual = actualCompletionList.items.find(
      (actual) =>
        actual.label === expected.label &&
        (!actual.kind || actual.kind === expected.kind),
    );

    assert.ok(actual, `"${expected.label}" not found in completions.`);
  });

  excludes?.forEach((expected, i) => {
    const actual = actualCompletionList.items.find(
      (actual) =>
        actual.label === expected.label &&
        (!actual.kind || actual.kind === expected.kind),
    );

    assert.equal(
      actual,
      undefined,
      `"${expected.label}" should not be among the completions.`,
    );
  });
}
