type t =
  | Illegal
  | Integer of int 
  | String of string
  | LeftBrace
  | RightBrace
  | Comma
  | Colon
  | LeftBracket
  | RightBracket

let to_str tok =
  match tok with
  | Illegal -> "ILLEGAL"
  | String content -> "STRING @ \"" ^ content ^ "\""
  | Integer value -> "INTEGER @ " ^ Stdlib.string_of_int value
  | LeftBrace -> "LBRACE"
  | RightBrace -> "RBRACE"
  | LeftBracket -> "LBRACKET"
  | RightBracket -> "RBRACKET"
  | Comma -> "COMMA"
  | Colon -> "COLON"
;;
