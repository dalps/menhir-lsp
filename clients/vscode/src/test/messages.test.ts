import assert from "assert";
import * as vscode from "vscode";
import { activate, getDocUri, P, R, rangeEqual } from "./helper";

suite("should provide navigational facilities in .messages files", () => {
  const uri = getDocUri("ParserMessages.messages");

  test("produces folding ranges for every entry", async () => {
    await activate(uri);
    const folds = (await vscode.commands.executeCommand(
      "vscode.executeFoldingRangeProvider",
      uri,
    )) as vscode.FoldingRange[];

    assert.equal(folds.length, 50);
    assert.equal(folds[1].start, 9, "Start line does not match");
    assert.equal(folds[1].end, 30, "End line does not match");
    assert.equal(folds[44].start, 569, "Start line does not match");
    assert.equal(folds[44].end, 588, "End line does not match");
  });

  test("provides document symbols", async () => {
    await activate(uri);
    const symbols = (await vscode.commands.executeCommand(
      "vscode.executeDocumentSymbolProvider",
      uri,
    )) as vscode.DocumentSymbol[];

    assert.equal(
      symbols[0].name,
      "1. Either a declaration or '%%' is expected at this point.",
    );
    assert.equal(
      symbols[44].name,
      "45. After a semicolon, an expression is expected.",
    );
    assert.ok(rangeEqual(symbols[44].range, R(P(570, 1), P(589, 1))));
    assert.ok(rangeEqual(symbols[44].selectionRange, R(P(583, 1), P(589, 1))));
  });

  test("can jump to the next entry", async () => {
    await activate(uri);
    await vscode.commands.executeCommand(
      "menhir-lsp-client.nextMessage",
      uri,
      P(576, 26),
    );

    const selection = vscode.window.activeTextEditor?.selection;

    assert.ok(
      selection,
      "Should have moved the seleciton onto the next message",
    );
    assert.ok(
      selection && (R(selection.start, selection.end), R(P(594, 1), P(598, 1))),
    );
  });
});
