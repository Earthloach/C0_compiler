type t = int
[@@deriving eq, ord, show]

let counter = ref 1
let reset () = counter := 1

let create () =
  let t = !counter in
  incr counter;
  t
;;

let name t = "%t" ^ string_of_int t