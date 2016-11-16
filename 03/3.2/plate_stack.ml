type 'a t = { mutable s : 'a list }            (* <1> *)

let empty () = { s = [] }
(* Initialize the stack with an empty list *)

let push item stack =
  stack.s <- item :: stack.s

let pop stack =
  match stack.s with
  | [] -> None
  | hd::tl ->
      stack.s <- tl;
      Some hd

let peek stack =
  match stack.s with
  | [] -> None
  | hd::tl -> Some hd
