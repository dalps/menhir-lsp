import assert from "assert";
import * as vscode from "vscode";
import { Range } from "vscode";
import { activate, getDocUri, P, R, rangeEqual, showRange } from "./helper";

suite("should provide navigational facilities in .messages files", () => {
  const uriParserMessages = getDocUri("ParserMessages.messages");
  const uriOriginalParser = getDocUri("original_parser.messages");

  test("produces folding ranges for every entry", async () => {
    await activate(uriParserMessages);
    const folds = (await vscode.commands.executeCommand(
      "vscode.executeFoldingRangeProvider",
      uriParserMessages,
    )) as vscode.FoldingRange[];

    assert.equal(folds.length, 50);
    assert.equal(folds[1].start, 9, "Start line does not match");
    assert.equal(folds[1].end, 30, "End line does not match");
    assert.equal(folds[44].start, 569, "Start line does not match");
    assert.equal(folds[44].end, 588, "End line does not match");
  });

  test("provides document symbols", async () => {
    await activate(uriParserMessages);
    const symbols = (await vscode.commands.executeCommand(
      "vscode.executeDocumentSymbolProvider",
      uriParserMessages,
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

  test("can jump to the next entry", () =>
    testJumpCommand(
      "menhir-lsp-client.nextMessage",
      uriParserMessages,
      P(575, 26),
      R(P(594, 1), P(598, 1)),
    ));

  test("can jump to the previous entry", () =>
    testJumpCommand(
      "menhir-lsp-client.previousMessage",
      uriParserMessages,
      P(575, 26),
      R(P(561, 1), P(567, 1)),
    ));

  test("can jump to the next dummy entry", () =>
    testJumpCommand(
      "menhir-lsp-client.nextDummyMessage",
      uriOriginalParser,
      P(25, 15),
      R(P(53, 1), P(54, 1)),
    ));

  test("can jump to the previous dummy entry", () =>
    testJumpCommand(
      "menhir-lsp-client.previousDummyMessage",
      uriOriginalParser,
      P(121, 3),
      R(P(53, 1), P(54, 1)),
    ));
});

async function testJumpCommand(
  command: string,
  uri: vscode.Uri,
  pos: vscode.Position,
  expected: Range,
  msg?: string | Error,
) {
  await activate(uri);

  if (!vscode.window.activeTextEditor) throw Error("No active editor!");

  vscode.window.activeTextEditor.selection = new vscode.Selection(pos, pos);

  await vscode.commands.executeCommand(command);

  // Remove this by making the server wait for the request to fulfill
  await new Promise((resolve, _reject) => {
    setTimeout(resolve, 2000);
  });

  // The command should have moved the selection, let's inspect it.
  const selection = vscode.window.activeTextEditor.selection;

  assert.ok(selection, msg);

  const actual = R(selection.start, selection.end);
  assert.ok(
    selection && rangeEqual(expected, actual),
    `Range mismatch: ${showRange(expected)} vs ${showRange(actual)}`,
  );
}
