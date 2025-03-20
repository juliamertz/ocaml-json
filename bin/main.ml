open Json_parser
open Stdio

let () =
  let content = In_channel.with_file "test.json" ~f:(fun ic -> In_channel.input_all ic) in
  let instance = Lexer.init content in
  let rec loop lexer =
    match Lexer.read_token lexer with
    | lexer, Some token ->
      printf "%s\n" (Token.to_str token);
      loop (Lexer.advance lexer)
    | _, None -> ()
  in
  loop instance
;;
