[@@@warnerror "-unused-value-declaration"]

open Base
open Stdio

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

let show lexer =
  printf "Lexer:\n";
  printf "Position: %d\n" lexer.position;
  printf "Current: %c\n" (Option.value ~default:'0' lexer.ch);
  printf "Content: %s\n" lexer.content
;;

let advance lexer =
  if lexer.position >= String.length lexer.content - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.content position) })
;;

let is_number ch = Char.is_digit ch

let is_number_opt ch =
  match ch with
  | Some ch -> is_number ch
  | None -> false
;;

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
  (* TODO: assert that current ch is a double qoute *)
  let lexer = advance lexer in
  let rec loop acc lexer =
    match lexer.ch with
    | Some '"' | None -> lexer, acc
    | Some ch -> loop (ch :: acc) (advance lexer)
  in
  let lexer, string = loop [] lexer in
  lexer, Token.String (String.of_char_list (List.rev @@ string))
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
      | ch ->
        printf "Unhandled char: %c\n" ch;
        lexer, Illegal
    in
    lexer, Some tok
;;

(* do the lexing *)

let read_entire_file filename =
  In_channel.with_file filename ~f:(fun ic -> In_channel.input_all ic)
;;

let () =
  let content = read_entire_file "test.json" in
  let instance = init content in
  let () = show instance in
  let rec loop lexer =
    match read_token lexer with
    | lexer, Some token ->
      printf "%s" (Token.to_str token);
      loop (advance lexer)
    | _, None -> ()
  in
  loop instance
;;

(*let instance, content = read_string instance in*)
(*let () = printf "val: %s\n" content in*)
(*printf*)
(*  "curr: %c\n"*)
(*  (match instance.ch with*)
(*   | Some ch -> ch*)
(*   | None -> ' ')*)
