open Menhir_lsp_lib.Utils
open Alcotest

let test_find_prefix () =
  let text =
    {|rule_S:
  "a" rule_A "c" {}

rule_A:
  | rule_A "b" "b" {}
  | "b" {}|}
  in
  let len = String.length text in
  let check msg ofs =
    check (triple int int string) msg (find_prefix text ofs)
  in
  check "should find a nonempty prefix here" 19 (14, 5, "rule_");
  check "should not find a prefix here" 49 (49, 0, "");
  check "should not find a prefix here" 68 (68, 0, "");
  check "should not find a prefix at the edges" len (len, 0, "");
  check "should not find a prefix at the edges" 0 (0, 0, "")

let () =
  Alcotest.run "Menhir_lsp_lib"
    [ ("Prefix search", [ test_case "find_prefix" `Quick test_find_prefix ]) ]
