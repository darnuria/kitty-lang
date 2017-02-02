(*
%token <float> FLOAT
%token <binary> BINARY
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACE
%token RIGHT_BRACK

%token ASSIGN
%token SEMICOLON
%token LET

%token EOL
*)

%token <int> INT
%token TRUE FALSE
%token <string> ID

%token LEFT_PARENS RIGHT_PARENS

%token DIVIDE PLUS MINUS TIMES MODULO

%token EOF
(*
 Precedency
 0: *
 1: /
 2: +
 4: -
 *)

%left PLUS MINUS
%left TIMES DIVIDE MODULO

%start <Ast.expr option> prog
%%

prog:
  | e = expression EOF { Some e };
  | EOF                { None };

(*
stm:
  | stm SEMICOLON stm
*)

%inline binop:
  | PLUS   { Ast.Add }
  | MINUS  { Ast.Sub }
  | TIMES  { Ast.Mult }
  | DIVIDE { Ast.Div }
  | MODULO { Ast.Mod }

expression:
  | TRUE { Ast.True }
  | FALSE { Ast.False }
  | i  = INT { Ast.Int i }
  | id = ID  { Ast.Id id }
  | left = expression; op = binop; right = expression
    { Ast.BinOp (left, op, right) }
  | LEFT_PARENS; e = expression ;RIGHT_PARENS
    { e }
(*
| LET; ID; EQUALS; expr = expr; SEMICOLON
*)

(* Inline pour eviter un shift/reduce conflict *)

