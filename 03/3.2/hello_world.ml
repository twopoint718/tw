(* Print a prompt messsage and then a newline and a
   prompt character '>'
*)
let prompt_for_number msg =                            (* <1> *)
  print_endline msg;
  print_string "> ";
  read_int ()

(* Run the program *)
let () =                                               (* <2> *)
  let num = prompt_for_number "Enter a number 1-5" in  (* <3> *)
  let greetee = match num with                         (* <4> *)
    | 1 -> "Doctor Watson"
    | 2 -> "Ambassador Spock"
    | 3 -> "Captian Nemo"
    | 4 -> "Admiral Ackbar"
    | 5 -> "Mister Jones"
    | _ -> "World"
  in
  print_endline ("Hello, " ^ greetee ^ "!")            (* <5> *)
