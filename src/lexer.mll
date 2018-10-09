{
open Lexing
open Parser

exception SyntaxError of (string)

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with pos_bol = lexbuf.lex_curr_pos;
             pos_lnum = pos.pos_lnum + 1
  }
  (*
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | white
 *)
}

let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let int = '-'? ['0'-'9']* (*[^ 'a'-'z' 'A'-'Z']*)
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
(* let invalidId = '-'? ['0'-'9']+(white) *)

(*
| "let" { LET }
| '=' { EQUALS }
*)
(* lexbuf available in rules *)
rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }

  (* | ',' { COMMA } *)
  | ';'  { SEMICOLON }
  | '{'  { LEFT_BRACE }
  | '}'  { RIGHT_BRACE }
  | '('  { LEFT_PARENS }
  | ')'  { RIGHT_PARENS }
  | '*'  { TIMES }
  | '/'  { DIVIDE }
  | '-'  { MINUS }
  | '+'  { PLUS }
  | '='  { EQUALS }
  | '>'  { GREATER }
  | '<'  { LESSER }
  | "<=" { LESSER_EQUAL}
  | ">=" { GREATER_EQUAL }
  (* | ":=" { ASSIGN } *)
  | "!=" { NOT_EQUALS }

  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* | fun     { FUNCTION } *)
  (* | match   { MATCH } *)
  (* | enum    { TYPE_SUM } *)
  (* | record { RECCORD_STRUCT } *)
  | "if"    { IF }
  | "let"   { LET }
  | "in"    { IN }
  | "fn"    { LAMBDA }
  | "fun"   { FUNCTION }
  | "end"   { END }
  | "mod"   { MODULO }
  | "else"  { ELSE }
  | "when"  { WHEN }
  | "()"    { UNIT }
  | "True"  { TRUE }
  | "False" { FALSE }
  | id      { ID  (Lexing.lexeme lexbuf) }
  | eof     { EOF }
  | _ { raise (SyntaxError ("Unexepected character: " ^ Lexing.lexeme lexbuf)) }

