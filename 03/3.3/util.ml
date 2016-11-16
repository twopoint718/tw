let rec take i lst =
  match lst with
  | [] -> []
  | (hd::tl) ->
      if i <= 0
      then []
      else hd :: take (i-1) tl

let rec drop i lst =
  match lst with
  | [] -> []
  | (hd::tl) ->
      if i <= 0
      then hd :: drop (i-1) tl
      else drop (i-1) tl

let remove i lst =
  take (i-1) lst @ drop i lst

let rec range i j =
  if i > j
  then []
  else i :: range (i+1) j


let string_of_meth = function
  | `GET -> "GET"
  | `POST -> "POST"
  | _ -> "???"
