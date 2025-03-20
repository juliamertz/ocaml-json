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
  | String content -> "\"" ^ content ^ "\""
  | Integer value -> Stdlib.string_of_int value
  (*| Integer value -> value*)
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | Comma -> ","
  | Colon -> ":"
;;
