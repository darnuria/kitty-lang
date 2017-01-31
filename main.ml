open Lexing
open Printf
open Ast
open Lexer
open Parser

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let ast = Parser.prog Lexer.read lexbuf in
  try ast with
  | Lexer.SyntaxError msg -> (
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  )
  | Parser.Error -> (
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
  exit(-1)
  )

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some exp -> (
    Printf.printf "Processed: %s\n" (Ast.output_expr exp);
    parse_and_print lexbuf
  )
  | None -> Printf.printf "End of expr.\n";()


let () =
  let channel = stdin in
  let rec loop () =
    let lexbuf = Lexing.from_channel channel in
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with pos_fname = "stdin"
    };
    parse_and_print lexbuf;
    loop ()
  in loop ()
