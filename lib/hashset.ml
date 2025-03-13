type 'a set = ('a, unit) Hashtbl.t

let set_add set thing = Hashtbl.add set thing ()

let set_contains set thing =
  try
    Hashtbl.find set thing;
    true
  with Not_found -> false

let set_length set = Hashtbl.length set

let set_of_list list =
  let set = Hashtbl.create (List.length list) in
  List.iter (fun e -> set_add set e) list;
  set
