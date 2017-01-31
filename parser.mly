(*
%token <float> FLOAT
%token <boolean> BOOL
%token <binary> BINARY
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACE
%token RIGHT_BRACK

%token LEFT_PARENS
%token RIGHT_PARENS

%token ASSIGN
%token SEMICOLON
%token LET

%token EOL
*)

%token <int> INT

%token DIVIDE
%token PLUS
%token MINUS
%token TIMES
%token <string> ID
%token EOF



(*
 Precedency
 0: *
 1: /
 2: +
 4: -
 *)

%left PLUS
%left TIMES
%left MINUS
%left DIVIDE

%start <Ast.expr option> prog
%%

prog:
  | e = expression { Some e };
  | EOF      { None };

expression:
  | left = expression; op = binop; right = expression
    { Ast.BinOp (left, op, right) }
  | i  = INT { Printf.fprintf stderr "Ast.Int lu %d\n" i; Ast.Int i }
  | id = ID  { Ast.Id id }
(*
  | LEFT_PARENS; e = expression ;RIGHT_PARENS
    {  }
 *)
(*
| LET; ID; EQUALS; expr = expr; SEMICOLON
*)

(* Inline pour eviter un shift/reduce conflict *)
%inline binop:
  | PLUS   { Ast.Add }
  | MINUS  { Ast.Sub }
  | TIMES  { Ast.Mult }
  | DIVIDE { Ast.Div }
