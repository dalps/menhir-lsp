import * as vscode from "vscode";
import { Position, Range } from "vscode";
import { activate, getDocUri, P, rangeEqual, uriEqual } from "./helper";
import assert from "assert";

const wordAt = (pos: Position, word: string) =>
  new Range(pos, new Position(pos.line, pos.character + word.length));

suite("should provide definitions in a lexer", () => {
  const uri = getDocUri("Lexer.mll");

  test("jumps to the definition of a lexer entrypoint", async () => {
    await testDefinition(uri, P(574, 7), {
      uri,
      range: wordAt(P(504, 6), "main"),
    });
  });

  test("jumps to the declaration of a rule's parameter (`openingrange`)", async () => {
    await testDefinition(uri, P(747, 23), {
      uri,
      range: wordAt(P(732, 17), "openingrange"),
    });
  });

  test("jumps to the declaration of a rule's parameter (`percent`)", async () => {
    await testDefinition(uri, P(684, 18), {
      uri,
      range: wordAt(P(681, 12), "percent"),
    });
  });

  test("jumps to the definition of a regexp binding defined in the same branch (`i`)", async () => {
    await testDefinition(uri, P(742, 52), {
      uri,
      range: wordAt(P(741, 22), "i"),
    });
  });

  test("jumps to the declaration of a named regexp", async () => {
    await testDefinition(uri, P(505, 12), {
      uri,
      range: wordAt(P(478, 5), "identchar"),
    });
  });

  test("jumps to the definition of a branch-local regexp binding (`directive`)", async () => {
    await testDefinition(uri, P(507, 71), {
      uri,
      range: wordAt(P(505, 22), "directive"),
    });
  });

  test("jumps to the definition of an OCaml symbol local to the action", async () => {
    await testDefinition(uri, P(581, 34), {
      uri,
      range: wordAt(P(578, 11), "identchar"),
    });
  });

  test("jumps to the definition of a OCaml symbol defined way back in the prelude", async () => {
    await testDefinition(uri, P(581, 12), {
      uri,
      range: wordAt(P(457, 5), "store_comment"),
    });
  });

  test("jumps to the definition of a deeply nested regexp binding (`where`)", async () => {
    await testDefinition(uri, P(703, 79), {
      uri,
      range: wordAt(P(490, 20), "where"),
    });
  });

  test("jumps to the definition of a deeply nested regexp binding (`i`)", async () => {
    await testDefinition(uri, P(703, 89), {
      uri,
      range: wordAt(P(483, 22), "i"),
    });
  });

  test("already on the definition, so it jumps back to the same symbol", async () => {
    await testDefinition(uri, P(486, 10), {
      uri,
      range: wordAt(P(486, 5), "poskeyword"),
    });
  });
});

async function testDefinition(
  uri: vscode.Uri,
  pos: Position,
  expected: vscode.Location,
) {
  await activate(uri);

  const locations = (await vscode.commands.executeCommand(
    "vscode.executeDefinitionProvider",
    uri,
    pos,
  )) as vscode.Location[];

  const definition = locations.at(0);

  assert.ok(
    definition &&
      uriEqual(definition.uri, expected.uri) &&
      rangeEqual(definition.range, expected.range),
    definition
      ? `Definition does not match expected location: expected ${expected.range}, actual ${definition.range}`
      : `Definition not found`,
  );
}
