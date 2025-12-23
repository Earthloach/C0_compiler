type t =
  { name : string
  ; unique_id : int
  }

(* Intern table: name -> symbol *)
let cache : (string, t) Hashtbl.t = Hashtbl.create 251
let next_unique_id = ref 0

let symbol (name : string) : t =
  match Hashtbl.find_opt cache name with
  | Some t -> t
  | None ->
      let unique_id = !next_unique_id in
      incr next_unique_id;
      let t = { name; unique_id } in
      Hashtbl.add cache name t;
      t

let name (x : t) : string = x.name

let compare (a : t) (b : t) : int = Int.compare a.unique_id b.unique_id
let equal (a : t) (b : t) : bool = a.unique_id = b.unique_id
let hash (x : t) : int = Hashtbl.hash x.unique_id

let pp (fmt : Format.formatter) (x : t) : unit = 
  Format.fprintf fmt "<symbol:%s %d>" x.name x.unique_id

module Ord = struct
  type nonrec t = t
  let compare = compare
end

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)