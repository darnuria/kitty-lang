{
open Lexing
open Parser

exception SyntaxError of string

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
let invalidId = '-'? ['0'-'9']+(white)

(* lexbuf available in rules *)
rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id  { ID  (Lexing.lexeme lexbuf)}
      (*
  | '(' { LEFT_PARENS }
  | ')' { RIGHT_PARENS }
  | "let" { LET }
  | '=' { EQUALS }
  | ';' { SEMICOLON }
         *)
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '-' { MINUS }
  | '+' { PLUS }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexepected character: " ^ Lexing.lexeme lexbuf)) }
