type id = string
type pos = int
type binOp = Mult | Sub | Add | Div
type stmt = Bind of (id * expr)
and expr =
  | Float of float
  | Int of int
  | Id of id
  | BinOp of (expr * binOp * expr)
val output_binOp : binOp -> char
val output_expr : expr -> string
val output_stmt : stmt -> string
