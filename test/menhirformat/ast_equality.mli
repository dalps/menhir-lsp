module Mly = MenhirSyntax
module Fmt_mly = Menhirformat_lib.Menhir
module Mll = OcamllexSyntax
module Fmt_mll = Menhirformat_lib.Ocamllex

val test_mly_string : string -> bool
val test_mll_string : string -> bool
