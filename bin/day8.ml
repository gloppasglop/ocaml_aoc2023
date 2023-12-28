open Base
open Stdio
open Angstrom

let file = "day8.txt"
let data = In_channel.read_all file
let whitespace = take_while Char.is_whitespace
let eol = string "\n"

let location =
  take_while (function
    | 'A' .. 'Z' -> true
    | '0' .. '9' -> true
    | _ -> false)
;;

let instructions =
  take_while (function
    | 'R' -> true
    | 'L' -> true
    | _ -> false)
;;

let map_parse =
  let* key = location in
  let* _ = whitespace in
  let* _ = char '=' in
  let* _ = whitespace in
  let* _ = char '(' in
  let* _ = whitespace in
  let* left = location in
  let* _ = whitespace in
  let* _ = char ',' in
  let* _ = whitespace in
  let* right = location in
  let* _ = char ')' in
  return (key, left, right)
;;

let parse =
  let* instructions = instructions in
  let* _ = eol in
  let* _ = eol in
  let* maps = sep_by1 eol map_parse in
  return (instructions, maps)
;;

let nodes_map = Hashtbl.create (module String)

let instructions =
  match parse_string ~consume:Prefix parse data with
  | Ok (instructions, maps) ->
    List.iter maps ~f:(fun (key, left, right) ->
      let _ = Hashtbl.add nodes_map ~key ~data:(left, right) in
      ());
    instructions
  | Error _ -> failwith "Error parsing"
;;

let next instruction node nodes =
  match instruction with
  | 'R' -> snd (Hashtbl.find_exn nodes node)
  | 'L' -> fst (Hashtbl.find_exn nodes node)
  | _ -> failwith "Incorrect instruction"
;;

let rec apply instructions count start_node nodes =
  match Sequence.next instructions with
  | Some (instruction, instructions) ->
    let next_node = next instruction start_node nodes in
    if String.equal next_node "ZZZ"
    then next_node, count
    else apply instructions (count + 1) next_node nodes
  | None -> failwith "Impossible"
;;

let rec apply' instructions count start_node nodes =
  match Sequence.next instructions with
  | Some (instruction, instructions) ->
    let next_node = next instruction start_node nodes in
    if String.is_suffix next_node ~suffix:"Z"
    then next_node, count
    else apply' instructions (count + 1) next_node nodes
  | None -> failwith "Impossible"
;;

(*
   let rec apply_multiples instructions count start_nodes nodes =
   let start_node_count = List.length start_nodes in
   match Sequence.next instructions with
   | Some (instruction, instructions) ->
   let next_nodes = List.map start_nodes ~f:(fun node -> next instruction node nodes) in
   let count_final =
   List.count next_nodes ~f:(fun node -> String.is_suffix node ~suffix:"Z")
   in
   if count_final > 0 then printf "Count : %d %d/%d\n" count count_final start_node_count;
   if count_final = start_node_count
   then next_nodes, count
   else (
   printf "Count %d: Instruction %c Start" count instruction;
   List.iter start_nodes ~f:(printf " %s,");
   printf " Next ";
   List.iter next_nodes ~f:(printf " %s,");
   print_endline "";
   apply_multiples instructions (count + 1) next_nodes nodes)
   | None -> failwith "Impossible"
   ;;
*)

let instructions_seq = Sequence.cycle_list_exn (String.to_list instructions)

let part1 nodes_map =
  let start_node = "AAA" in
  let _, count = apply instructions_seq 1 start_node nodes_map in
  printf "Part 1: %d\n" count
;;

let () = part1 nodes_map
let get_all_start nodes = Hashtbl.filter_keys nodes ~f:(String.is_suffix ~suffix:"A")
let rec gcd u v = if v <> 0 then gcd v (u % v) else abs u

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / gcd m n
;;

let () =
  let start_nodes = Hashtbl.keys (get_all_start nodes_map) in
  let results =
    List.map start_nodes ~f:(fun start_node ->
      let _, count = apply' instructions_seq 1 start_node nodes_map in
      count)
  in
  let result = List.fold results ~init:1 ~f:(fun acc result -> lcm acc result) in
  printf "Part2: %d\n" result
;;
