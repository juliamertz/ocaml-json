open Json_parser
open Stdio

let content = In_channel.with_file "test.json" ~f:(fun ic -> In_channel.input_all ic)

let () =
  let lexer = Lexer.init content in
  let _ = Parser.deserialize_value lexer in
  ()
;;

let () =
  let tokens = Lexer.tokens_from_str content in
  let rec print_tokens = function
    | [] -> ()
    | head :: tail ->
      printf "%s\n" (Token.to_str head);
      print_tokens tail
  in
  print_tokens tokens
;;
