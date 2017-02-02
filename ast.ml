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
  (* Statements/Instruction.
   * Return ()
   * like assignation
   * let <ID> = <Expr>;
   * Other design ML-style.
   * let <ID> = <Expr> in <Expr>
  *)


let output_binOp = function
  | Mult -> "*"
  | Sub  -> "-"
  | Add  -> "+"
  | Div  -> "/"
  | Mod  -> "mod"

let rec output_expr = function
  | False   -> "false"
  | True    -> "True"
  | Int i   -> string_of_int i
  | Float f -> string_of_float f
  | Id id   -> id
  | BinOp(expr_left, binOp, expr_right) -> (
      output_expr  (expr_left)
      ^ output_binOp binOp
      ^ output_expr  (expr_right)
    )

let output_stmt = function
  | Bind(id, expr) ->
    Printf.sprintf "let %s = %s;" (output_expr expr) id
