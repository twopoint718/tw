open Plate_stack
open OUnit2

(* Helper for successful result *)
let assert_success () = assert_equal 1 1

let empty_stack test_context =
  let stack = empty () in
  match peek stack with
  | Some _ -> assert_failure "the stack should be empty"
  | None -> assert_success ()

let push_pop test_context =
  let expected = 5 in
  let stack = empty () in
  push expected stack;
  match pop stack with
  | Some actual -> assert_equal expected actual
  | None -> assert_failure "pop didn't return anything"

let suite =
  "suite" >:::
  [
    "empty creates an empty stack" >:: empty_stack;
    "push then pop is identity" >:: push_pop
  ]

let () =
  run_test_tt_main suite
