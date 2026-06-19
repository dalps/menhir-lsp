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

let test_free_variables () =
  let vars input =
    input |> Lexing.from_string |> Ppxlib.Parse.implementation
    |> Menhir_lsp_lib.OcamlSymbols.get_fvars
    |> L.map
         (fun
           ({ txt; loc = { loc_start = { pos_lnum; _ }; _ } } :
             string Ppxlib.Loc.t)
         ->
           (* Discriminate many occurrences with the line number *)
           (txt, pos_lnum))
  in
  let check_vars inp ~includes ~excludes =
    let vars = vars inp in
    List.iter (fun ((txt, _) as v) ->
        check bool
          (spr "List does include free variable: `%s`" txt)
          (L.mem v vars) true)
    @@ includes;
    List.iter (fun ((txt, ln) as v) ->
        check bool
          (spr
             "List does not include any local variable bound by let .. in: \
              `%s` at line %d"
             txt ln)
          (L.mem v vars) false)
    @@ excludes
  in
  check_vars
    {|let re, r = regexp in
      let res = { name; regexp = re } in
      Hashtbl.add named_regexps name.v @@ (locate $loc res, r);
      res|}
    ~includes:[ ("regexp", 1); ("name", 2) ]
    ~excludes:[ ("regexp", 2); ("re", 2); ("r", 3); ("res", 3) ];
  check_vars
    {|let cls, cset = cls in
    locate $loc @@ CharSet cls, Characters cset|}
    ~includes:[ ("cls", 1) ]
    ~excludes:[ ("cls", 2); ("cset", 2) ]

let () =
  Alcotest.run "Menhir_lsp_lib"
    [
      ("Prefix search", [ test_case "find_prefix" `Quick test_find_prefix ]);
      ( "Free variable extraction",
        [ test_case "free_variables" `Quick test_free_variables ] );
    ]
