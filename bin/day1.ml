open Base
open Stdio

let search1 = Hashtbl.create (module String);;

Hashtbl.add search1 ~key:"1" ~data:1;;
Hashtbl.add search1 ~key:"2" ~data:2;;
Hashtbl.add search1 ~key:"3" ~data:3;;
Hashtbl.add search1 ~key:"4" ~data:4;;
Hashtbl.add search1 ~key:"5" ~data:5;;
Hashtbl.add search1 ~key:"6" ~data:6;;
Hashtbl.add search1 ~key:"7" ~data:7;;
Hashtbl.add search1 ~key:"8" ~data:8;;
Hashtbl.add search1 ~key:"9" ~data:9

let search2 = Hashtbl.create (module String);;

Hashtbl.add search2 ~key:"1" ~data:1;;
Hashtbl.add search2 ~key:"one" ~data:1;;
Hashtbl.add search2 ~key:"2" ~data:2;;
Hashtbl.add search2 ~key:"two" ~data:2;;
Hashtbl.add search2 ~key:"3" ~data:3;;
Hashtbl.add search2 ~key:"three" ~data:3;;
Hashtbl.add search2 ~key:"4" ~data:4;;
Hashtbl.add search2 ~key:"four" ~data:4;;
Hashtbl.add search2 ~key:"5" ~data:5;;
Hashtbl.add search2 ~key:"five" ~data:5;;
Hashtbl.add search2 ~key:"6" ~data:6;;
Hashtbl.add search2 ~key:"six" ~data:6;;
Hashtbl.add search2 ~key:"7" ~data:7;;
Hashtbl.add search2 ~key:"seven" ~data:7;;
Hashtbl.add search2 ~key:"8" ~data:8;;
Hashtbl.add search2 ~key:"eight" ~data:8;;
Hashtbl.add search2 ~key:"9" ~data:9;;
Hashtbl.add search2 ~key:"nine" ~data:9

let first s search =
  Hashtbl.fold
    ~init:(None, 0)
    ~f:(fun ~key ~data (pos, value) ->
      let index = String.substr_index s ~pattern:key in
      match index with
      | None -> pos, value
      | Some idx ->
        (match pos with
         | None -> Some idx, data
         | Some pos' -> if idx < pos' then Some idx, data else pos, value))
    search
;;

let result data search =
  List.fold
    ~init:0
    ~f:( + )
    (List.map
       ~f:(fun s ->
         let _, value = first s search in
         value)
       data)
;;

let file = "day1_1.txt"
let data = In_channel.read_lines file
let data' = List.map ~f:String.rev data
let f = result data search1
let l = result data' search1
let () = printf "Result 1 : %d\n" ((f * 10) + l)
let file = "day1_2.txt"
let data = In_channel.read_lines file
let data' = List.map ~f:String.rev data
let f = result data search2
let search2' = Hashtbl.create (module String)

(* create a hastbl with same values as search2 but keys are reversed*)
let () =
  Hashtbl.iteri
    ~f:(fun ~key ~data ->
      let _ = Hashtbl.add search2' ~key:(String.rev key) ~data in
      ())
    search2
;;

let l = result data' search2'
let () = printf "Result 2 : %d\n" ((f * 10) + l)
