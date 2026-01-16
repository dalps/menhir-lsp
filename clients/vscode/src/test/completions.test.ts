import * as vscode from "vscode";
import { Position, Range, Uri } from "vscode";
import * as assert from "assert";
import { activate, getDocUri } from "./helper";

suite("Should do completion", () => {
  const uri = getDocUri("grammar6.mly");

  test("Completes a token symbol", async () => {
    await testCompletion(uri, new Position(0, 0), {
      items: [
        { label: "Ta", kind: vscode.CompletionItemKind.Enum },
        { label: "Tb", kind: vscode.CompletionItemKind.Enum },
        { label: "Tc", kind: vscode.CompletionItemKind.Enum },
      ],
    });
  });
});

async function testCompletion(
  uri: Uri,
  pos: Position,
  expectedCompletionList: vscode.CompletionList
) {
  await activate(uri);

  const actualCompletionList = (await vscode.commands.executeCommand(
    "vscode.executeCompletionItemProvider",
    uri,
    pos
  )) as vscode.CompletionList;

  assert.ok(actualCompletionList.items.length >= 2);
  expectedCompletionList.items.forEach((expected, i) => {
    const actual = actualCompletionList.items[i];
    assert.equal(actual.label, expected.label);
    assert.equal(actual.kind, expected.kind);
  });
}
