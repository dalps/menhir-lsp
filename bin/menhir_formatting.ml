open Utils
open MenhirSyntax
open PPrint

include Comment_location.Make (struct
  type syntax = Syntax.main

  include Located
  include Range
end)

class formatter ~(notify_back : notify_back) ~(doc : Text_document.t) =
  let open Syntax in
  let _ = (notify_back, doc) in
  object (_self)
    inherit [_] ast_reduce
    method zero = empty
    method plus = ( ^^ )
    method text = string
  end
