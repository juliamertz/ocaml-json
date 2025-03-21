open Base
open Token

type error =
  | ExpectedIdentifier
  | ExpectedColon

type value =
  | String of string
  | Integer of int
  | Object of (string, value) Hashtbl.t
  | Array of value list

type field =
  { key : string
  ; value : value
  }

let deserialize_object lexer =
  match Lexer.read_token lexer with
  | _, Some (String _) -> Ok "TODO: Object"
  | _, Some tok -> Error ("Expected an identifier found: " ^ Token.to_str tok)
  | _, None -> Error "Expected an identifier found EOF"
;;

let deserialize_value (lexer : Lexer.t) =
  let value =
    match Lexer.read_token lexer with
    | lexer, Some token ->
      (match token with
       | LeftBrace -> deserialize_object lexer
       | LeftBracket -> Ok "TODO: Array"
       | _ -> Error "Unhandled")
    | _, None -> Error "NONE"
  in
  value
;;

(*let deserialize_binding _ lexer =*)
(*  match Lexer.read_token lexer with*)
(*  | _, Some Colon -> deserialize_value lexer *)
(*  | _, Some tok -> Error ("Expected colon found: " ^ Token.to_str tok)*)
(*  | _, None -> Error "Expected colon found EOF"*)
(*;;*)
