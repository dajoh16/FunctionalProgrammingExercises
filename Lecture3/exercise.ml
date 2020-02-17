
(* a datatype of arithmetic expressions *)
type aexp =
  | X
  | Lit of int
  | Plus of aexp * aexp
  | Times of aexp * aexp

let mytree = Plus (Lit 1, Times (X, Lit 3))

(* our interpreter of arithmetic expressions *)
let rec interpret xval ae = match ae with
  | X -> xval
  | Lit i -> i
  | Plus (ae0, ae1) ->
    let v0 = interpret xval ae0 in
    let v1 = interpret xval ae1 in
    v0 + v1
  | Times (ae0, ae1) ->
    let v0 = interpret xval ae0 in
    let v1 = interpret xval ae1 in
    v0 * v1

(*  interpret mytree;;  *)

let rec exp_to_string ae = match ae with
  | X -> "x"
  | Lit i -> string_of_int i
  | Plus (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "(" ^ s0 ^ "+" ^ s1 ^ ")"
  | Times (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "(" ^ s0 ^ "*" ^ s1 ^ ")"

let rec size ae = match ae with
  | X -> 1
  | Lit i -> 1
  | Plus (ae0, ae1) ->
    let s0 = size ae0 in 
    let s1 = size ae1 in
    s0 + s1 + 1
  | Times (ae0, ae1) ->
    let s0 = size ae0 in 
    let s1 = size ae1 in
    s0 + s1 + 1

