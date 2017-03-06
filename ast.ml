(*
   A faire:
   Floats, assignation,
   Ajouter les positions dans le fichier source.
*)

type pos = int
type id = string
and expr =
  | Unit
  | False
  | True
  (*| Float of float *)
  | Int of int
  | Id  of id
  | Op of (op * expr * expr)
  | Apply of (id * expr list)
  | If  of (expr * expr * expr)
  | Let of (id * expr * expr)
  | Seq of (expr * expr)
  | Fun of (id * id list * expr)
  | Lambda of (id list * expr)
(* Statements/Instruction.
 * Return ()
 * like assignation
 * Other design ML-style.
 * let <ID> = <Expr> in <Expr>; <- Mandatory
 *    ^              ^
 *    |              Explicit block
 *    Optional mut?
*)
and op =
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

let string_of_op = function
  | Mult -> "*"
  | Sub  -> "-"
  | Add  -> "+"
  | Div  -> "/"
  | Mod  -> "mod"

  | Lt   -> "<"
  | Gt   -> ">"
  | Ge   -> ">="
  | Le   -> "<="
  | Neq  -> "!="
  | Eq   -> "="

let string_of_id (identif: id) : string = identif

let rec string_of_expr = function
  | Unit    -> "()"
  | False   -> "False"
  | True    -> "True"
  | Int i   -> string_of_int i
  (*| Float f -> string_of_float f *)
  | Id id   -> string_of_id id
  | Op (op, left, right) ->
    (Printf.sprintf "(%s %s %s)"
       (string_of_expr left)
       (string_of_op op)
       (string_of_expr right))
  | Apply (ident, args) ->
    (Printf.sprintf "%s %s"
       (string_of_id ident)
       (List.map string_of_expr args
        |> String.concat " "))
  | If (pred, then', else') ->
    (Printf.sprintf "if %s { %s } else { %s }"
       (string_of_expr pred)
       (string_of_expr then')
       (string_of_expr else'))
  | Let (id, init, body) ->
    (Printf.sprintf "let %s = %s in %s end"
       id
       (string_of_expr init)
       (string_of_expr body))
  | Seq (left, right) ->
    (Printf.sprintf "%s ; %s"
       (string_of_expr left)
       (string_of_expr right))
  | Fun (id, args, body) ->
    (Printf.sprintf "fun %s %s {\n%s\n}"
       (string_of_id id)
       (List.map string_of_id args |> String.concat " ")
       (string_of_expr body))
  | Lambda (args, body) ->
    (Printf.sprintf "fn %s {\n%s\n}"
       (List.map string_of_id args |> String.concat " ")
       (string_of_expr body))


(* TODO: implem it really. *)
let string_of_program = string_of_expr
