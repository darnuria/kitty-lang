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

(* %token EOL *)

%token IF ELSE
%token WHEN

%token UNIT

%token LET
%token IN
%token END
%token FUNCTION
%token LAMBDA

%token DIVIDE PLUS MINUS TIMES MODULO
%token LESSER_EQUAL GREATER_EQUAL
%token GREATER LESSER
%token NOT_EQUALS EQUALS

%token EOF

%nonassoc SEMICOLON
%nonassoc EQUALS

%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left LESSER_EQUAL GREATER_EQUAL GREATER LESSER NOT_EQUALS

(*
%token ASSIGN
 *)

(*
 Precedency
 0: *
 1: /
 2: +
 4: -
*)

%start <Ast.expr option> program
%%

program:
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


fun_args:
  | (* end of args *) { [] }
  | arg = ID; args = fun_args
    { arg :: args }

(*
  (* { <expression> } *)
  | LEFT_BRACE; e = expression; RIGHT_BRACE
    { e }
  (* let <pattern> = <expression in <expression> end *)
*)
expression:
  | UNIT     { Ast.Unit }
  | TRUE     { Ast.True }
  | FALSE    { Ast.False }
  | i  = INT { Ast.Int (i) }
  | id = ID  { Ast.Id (id) }
  | left = expression; op = binop; right = expression
    { Ast.Op (op, left, right) }
  (* ( <expression> ) *)
  | LEFT_PARENS; e = expression ;RIGHT_PARENS
    { e }
  (* if <expression> { <expression> } else { <expression> } *)
  | IF; predicat = expression; LEFT_BRACE; ifExpr = expression; RIGHT_BRACE;
    ELSE; LEFT_BRACE; elseExpr = expression; RIGHT_BRACE
    { Ast.If (predicat, ifExpr, elseExpr) }
  (* when <expr> { <expression> } *)
  | WHEN; test = expression; LEFT_BRACE; whenExpr = expression; RIGHT_BRACE
    { Ast.If (test, whenExpr, Ast.Unit) }
  (* let <identifiant> = <expression> in expression end *)
  | LET; id = ID; EQUALS; dec = expression; IN; body = expression; END
    { Ast.Let (id, dec, body) }
  (* <expression>; <expression> *)
  | left = expression; SEMICOLON; right = expression
    { Ast.Seq (left, right) }
  (* fun <identifiant> <args_list> { <expression> } *)
  | FUNCTION; id = ID; args = fun_args; LEFT_BRACE;
    fun_body = expression; RIGHT_BRACE
   { Ast.Fun (id, args, fun_body)}
  (* lambda: fn <args_list> { <expression> } *)
  | LAMBDA; args = fun_args; LEFT_BRACE; fun_body = expression; RIGHT_BRACE
    { Ast.Lambda (args, fun_body) }


