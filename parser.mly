(*
%token <float> FLOAT
%token <binary> BINARY
%token LEFT_BRACKET RIGHT_BRACKET

%token ASSIGN
%token LET


Keyword version:
if boolExpr then expr else expr
Braces version:
if boolExpr { expr } else { expr }

Pro Imbrication simplifiée.
match expr {
 | pattern -> expr
}
match expr with
 |
*)

%token <int> INT
%token TRUE FALSE
%token <string> ID

%token LEFT_PARENS RIGHT_PARENS
%token LEFT_BRACE RIGHT_BRACE
%token SEMICOLON
%token EOL

%token IF ELSE
(*
 %token WHEN
 *)
%token LET
%token IN

%token DIVIDE PLUS MINUS TIMES MODULO
%token LESSER_EQUAL GREATER_EQUAL
%token GREATER LESSER
%token NOT_EQUALS EQUALS
(*
%token ASSIGN
 *)


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
  | EOF { None };
  | e = expression EOF { Some e };

(*
stm:
  | stm SEMICOLON stm
*)

%inline binop:
  | PLUS          { Ast.Add }
  | MINUS         { Ast.Sub }
  | TIMES         { Ast.Mult }
  | DIVIDE        { Ast.Div }
  | MODULO        { Ast.Mod }
  | LESSER_EQUAL  { Ast.Ge }
  | GREATER_EQUAL { Ast.Le }
  | GREATER       { Ast.Gt }
  | LESSER        { Ast.Le }
  | NOT_EQUALS    { Ast.Neq }
  | EQUALS        { Ast.Eq }


expression:
  | TRUE { Ast.True }
  | FALSE { Ast.False }
  | i  = INT { Ast.Int i }
  | id = ID  { Ast.Id id }
  | left = expression; op = binop; right = expression
    { Ast.BinOp (left, op, right) }
    (* Block Expression; scoping. *)
  | LEFT_BRACK; e = expression; RIGHT_BRACE
    { e }
  | LEFT_PARENS; e = expression ;RIGHT_PARENS
    { e }
  | IF; predicat = expression; LEFT_BRACE; ifExpr = expression; RIGHT_BRACE;
    ELSE; LEFT_BRACE; elseExpr = expression; RIGHT_BRACE
    { Ast.IfExpr(predicat, ifExpr, elseExpr) }
  | LET; id = ID; EQUALS; dec = expression; IN; body = expression;
    { Ast.LetExpr(id, dec, body) }
  | left = expression; SEMICOLON; right = expression;
    { Ast.SeqExpr(left, right)}


