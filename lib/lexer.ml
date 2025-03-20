open Base

type t =
  { content : string
  ; position : int
  ; ch : char option
  }

let init content =
  if String.is_empty content
  then { content; position = 0; ch = None }
  else { content; position = 0; ch = Some (String.get content 0) }
;;

let advance lexer =
  if lexer.position >= String.length lexer.content - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.content position) })
;;

let is_number ch = Char.is_digit ch

let rec skip_whitespace lexer =
  match lexer.ch with
  | Some ' ' | Some '\n' -> skip_whitespace (advance lexer)
  | _ -> lexer
;;

let peek_ch lexer =
  if lexer.position >= String.length lexer.content - 1
  then None
  else Some (String.get lexer.content (lexer.position + 1))
;;

let seek_ch lexer condition =
  let rec loop lexer = if condition lexer.ch then lexer else loop (advance lexer) in
  let lexer = loop lexer in
  lexer, lexer.position
;;

let read_string lexer =
  let lexer = advance lexer in
  let rec loop acc lexer =
    match lexer.ch with
    | Some '"' | None -> lexer, acc
    | Some ch -> loop (ch :: acc) (advance lexer)
  in
  let lexer, string = loop [] lexer in
  lexer, Token.String (String.of_char_list (List.rev @@ string))
;;

(* TODO: Remove this when read_integer is cleaned up *)
let is_number_opt ch =
  match ch with
  | Some ch -> is_number ch
  | None -> false
;;

(* This is a bit messy *)
let read_integer lexer =
  let rec loop acc lexer =
    match lexer.ch with
    | Some ch when is_number ch ->
      if is_number_opt (peek_ch lexer)
      then loop (ch :: acc) (advance lexer)
      else lexer, ch :: acc
    | _ -> lexer, acc
  in
  let lexer, string = loop [] lexer in
  lexer, Token.Integer (Stdlib.int_of_string @@ String.of_char_list (List.rev @@ string))
;;

let read_token lexer =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, tok =
      let open Token in
      match ch with
      | '{' -> lexer, LeftBrace
      | '}' -> lexer, RightBrace
      | '[' -> lexer, LeftBracket
      | ']' -> lexer, RightBracket
      | ',' -> lexer, Comma
      | ':' -> lexer, Colon
      | '"' -> read_string lexer
      | ch when Char.is_digit ch -> read_integer lexer
      | _ -> lexer, Illegal
    in
    lexer, Some tok
;;
