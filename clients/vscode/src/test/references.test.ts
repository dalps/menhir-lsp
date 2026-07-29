import * as vscode from "vscode";
import { Position, Range, Location } from "vscode";
import {
  activate,
  getDocUri,
  L,
  P,
  rangeEqual,
  uriEqual,
  wordAt,
} from "./helper";
import assert from "assert";

suite("should provide references in a lexer", () => {
  const uri = getDocUri("Lexer.mll");

  test("references of regexp binding", async () => {
    await testReferences(uri, P(610, 15), []);
  });

  test("references of rule parameter", async () => {
    await testReferences(uri, P(684, 16), []);
  });
});

suite("should provide references in calc_lexer.mll", () => {
  const uri = getDocUri("calc_lexer.mll");

  test("references of regexp binding", async () => {
    await testReferences(uri, P(6, 49), [
      { uri, range: wordAt(P(6, 47), "ending") },
      { uri, range: wordAt(P(16, 12), "ending") },
    ]);

    await testReferences(uri, P(13, 19), [
      { uri, range: wordAt(P(11, 26), "ending") },
      { uri, range: wordAt(P(13, 16), "ending") },
    ]);
  });
});

suite("should provide references in a parser", () => {
  const uri = getDocUri("Parser.mll");

  test("references of a branch producer", async () => {
    const at = (p: Position) => L(uri, wordAt(p, "x"));
    const start = P(756, 3);

    await testReferences(uri, start, [at(start), at(P(757, 20))]);
  });

  test("references of a branch producer (from a different location)", async () => {
    const at = (p: Position) => L(uri, wordAt(p, "x"));
    const start = P(757, 20);

    await testReferences(uri, start, [at(start), at(P(756, 3))]);
  });

  test("references of rule parameter `X`", async () => {
    const at = (p: Position) => L(uri, wordAt(p, "X"));
    const start = P(756, 7);

    await testReferences(uri, start, [at(start), at(P(755, 9))]);
  });

  test("references of rule parameter `X`", async () => {
    const at = (p: Position) => L(uri, wordAt(p, "X"));
    const start = P(744, 40);

    await testReferences(uri, start, [at(start), at(P(747, 56))]);
  });
});

async function testReferences(
  uri: vscode.Uri,
  pos: Position,
  expected: vscode.Location[],
) {
  await activate(uri);

  const refs = (await vscode.commands.executeCommand(
    "vscode.executeReferenceProvider",
    uri,
    pos,
  )) as vscode.Location[];

  assert.equal(
    expected.length,
    refs.length,
    `There are more or less references (${refs.length}) than expected (${expected.length})`,
  );

  expected.forEach((exp) => {
    assert.ok(
      refs.find(
        (ref) =>
          ref && uriEqual(ref.uri, exp.uri) && rangeEqual(ref.range, exp.range),
      ),
      `Expected reference ${exp.range} not included in server response`,
    );
  });
}
