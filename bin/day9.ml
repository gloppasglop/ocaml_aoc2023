open Base
open Stdio

let file = "day9.txt"
let data = In_channel.read_lines file

let parsed_data =
  List.map data ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
;;

let delta l =
  let result =
    match l with
    | [] -> []
    | hd :: tail ->
      let _, result =
        List.fold tail ~init:(hd, []) ~f:(fun (previous, acc) value ->
          value, (value - previous) :: acc)
      in
      result
  in
  List.rev result
;;

let rec differences acc l =
  let diff = delta l in
  if List.exists diff ~f:(fun n -> not (Int.equal n 0))
  then differences (diff :: acc) diff
  else acc
;;

let next_value history =
  let history = differences [ history ] history in
  let next_value =
    List.fold history ~init:0 ~f:(fun acc diff ->
      let last = List.last_exn diff in
      acc + last)
  in
  next_value
;;

let previous_value history =
  let history = differences [ history ] history in
  let previous_value =
    List.fold history ~init:0 ~f:(fun acc diff ->
      let first = List.hd_exn diff in
      first - acc)
  in
  previous_value
;;

let () =
  let history = List.map parsed_data ~f:next_value in
  let result = List.fold history ~init:0 ~f:( + ) in
  printf "Part 1: %d\n" result
;;

let () =
  let history = List.map parsed_data ~f:previous_value in
  let result = List.fold history ~init:0 ~f:( + ) in
  printf "Part 2:  %d\n" result
;;
