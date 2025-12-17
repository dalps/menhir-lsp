(** List of string pairs mapping grammar rule names to their docstrings.
  Generated from front/standard.mly with the help of GitHub Copilot.
*)
let menhir_standard_library_doc = CCHashtbl.of_list [
  ("endrule", "`endrule(X)` is the same as `X`.\n\nThis allows placing an anonymous subrule in the middle of a rule.");
  ("anonymous", "`anonymous(X)` is a deprecated synonym for `endrule(X)`.\n\nIt was never documented.");
  ("midrule", "`midrule(X)` is the same as `X`.\n\nThis allows placing an anonymous subrule in the middle of a rule.");
  ("embedded", "`embedded(X)` is a deprecated synonym for `midrule(X)`.\n\nIt was never documented.");
  ("option", "`option(X)` recognizes either nothing or `X`. It produces a value of type `'a option` if `X` produces a value of type `'a`.");
  ("ioption", "`ioption(X)` is identical to `option(X)`, except its definition is inlined. This has the effect of duplicating the production that refers to it, possibly eliminating an LR(1) conflict.");
  ("boption", "`boption(X)` recognizes either nothing or `X`. It produces a value of type `bool`.");
  ("loption", "`loption(X)` recognizes either nothing or `X`. It produces a value of type `'a list` if `X` produces a value of type `'a list`.");
  ("pair", "`pair(X, Y)` recognizes the sequence `X Y`. It produces a value of type `'a * 'b` if `X` and `Y` produce values of type `'a` and `'b`, respectively.");
  ("separated_pair", "`separated_pair(X, sep, Y)` recognizes the sequence `X sep Y`. It produces a value of type `'a * 'b` if `X` and `Y` produce values of type `'a` and `'b`, respectively.");
  ("preceded", "`preceded(opening, X)` recognizes the sequence `opening X`. It passes on the value produced by `X`, so that it produces a value of type `'a` if `X` produces a value of type `'a`.");
  ("terminated", "`terminated(X, closing)` recognizes the sequence `X closing`. It passes on the value produced by `X`, so that it produces a value of type `'a` if `X` produces a value of type `'a`.");
  ("delimited", "`delimited(opening, X, closing)` recognizes the sequence `opening X closing`. It passes on the value produced by `X`, so that it produces a value of type `'a` if `X` produces a value of type `'a`.");
  ("list", "`list(X)` recognizes a possibly empty list of `X`'s. It produces a value of type `'a list` if `X` produces a value of type `'a`. The front element of the list is the first element that was parsed.");
  ("nonempty_list", "`nonempty_list(X)` recognizes a nonempty list of `X`'s. It produces a value of type `'a list` if `X` produces a value of type `'a`. The front element of the list is the first element that was parsed.");
  ("separated_list", "`separated_list(separator, X)` recognizes a possibly empty list of `X`'s, separated with `separator`'s. It produces a value of type `'a list` if `X` produces a value of type `'a`. The front element of the list is the first element that was parsed.");
  ("separated_nonempty_list", "`separated_nonempty_list(separator, X)` recognizes a nonempty list of `X`'s, separated with `separator`'s. It produces a value of type `'a list` if `X` produces a value of type `'a`. The front element of the list is the first element that was parsed.");
  ("rev", "`rev(XS)` recognizes the same language as `XS`, but reverses the resulting OCaml list.");
  ("flatten", "`flatten(XSS)` recognizes the same language as `XSS`, and flattens the resulting OCaml list of lists.");
  ("append", "`append(XS, YS)` recognizes `XS YS`, and appends (concatenates) the resulting OCaml lists.");
]
