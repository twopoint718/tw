# Gossamer

Gossamer is a simple, functional, manifestly-typed language which compiles to
WebAssembly. It roughly corresponds to the simply-typed lambda calculus. It is
introduced as Chapter 4 of "The Transparent Web" by Christopher Wilson.


## Example

```ocaml
let z : int = 3 + (if 5 < 6 then 10 else 100) ;;

let x : int = 14 ;;

let fact : int -> int =
  fun f (n : int) : int {
    if n = 0 then 1 else n * f (n-1) } ;;

let main : int = fact 10 ;;
```
