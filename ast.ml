open Printf

(*
   A faire:
   Floats, assignation,
   Ajouter les positions dans le fichier source.
*)

type id = string
type pos = int
  (*
| Bool of bool
   *)
type binOp =
  | Mult
  | Sub
  | Add
  | Div
  | Mod
  | Le
  | Ge
  | Gt
  | Lt
  | Neq
  | Eq

type stmt =
  | Bind of (id * expr)
and expr =
  (* | Nil *)
  | False
  | True
  | Float of float
  | Int of int
  | Id of id
  | BinOp of (expr * binOp * expr)
  | IfExpr of (expr * expr * expr)
  | LetExpr of (id * expr * expr)
  | SeqExpr of (expr * expr)
  (* Statements/Instruction.
   * Return ()
   * like assignation
   * Other design ML-style.
   * let <ID> = <Expr> in <Expr>; <- Mandatory
   *    ^              ^
   *    |              Explicit block
   *    Optional mut?
  *)


let output_binOp = function
  | Mult -> "*"
  | Sub  -> "-"
  | Add  -> "+"
  | Div  -> "/"
  | Mod  -> "mod"
  | Le   -> "<"
  | Ge   -> ">"
  | Gt   -> ">="
  | Lt   -> "<="
  | Neq  -> "!="
  | Eq   -> "="

let rec output_expr = function
  | False   -> "False"
  | True    -> "True"
  | Int i   -> string_of_int i
  | Float f -> string_of_float f
  | Id id   -> id
  | BinOp(expr_left, binOp, expr_right) ->
    (Printf.sprintf "%s %s %s"
       (output_expr expr_left)
       (output_binOp binOp)
       (output_expr  expr_right))
  | IfExpr(pred, then', else') ->
    (Printf.sprintf "if %s { %s } else { %s }"
       (output_expr pred)
       (output_expr then')
       (output_expr else'))
  | LetExpr (id, init, body) ->
    (Printf.sprintf "let %s = %s in %s"
       id
       (output_expr init)
       (output_expr body))
  | SeqExpr (left, right) ->
    (Printf.sprintf "%s ; %s"
       (output_expr left)
       (output_expr right))


let output_stmt = function
  | Bind(id, expr) ->
    Printf.sprintf "let %s = %s;" (output_expr expr) id
