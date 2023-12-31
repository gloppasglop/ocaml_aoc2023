open Base
open Stdio

let file = "day11.txt"
let data = In_channel.read_lines file
let data = List.map data ~f:String.to_list

let empty_lines data =
  List.foldi data ~init:[] ~f:(fun i acc line ->
    if List.count line ~f:(Char.equal '#') = 0 then i :: acc else acc)
;;

let ys_to_expand = empty_lines data
let xs_to_expand = empty_lines (List.transpose_exn data)

let parse_data universe =
  let rec aux (y, acc) universe =
    match universe with
    | [] -> acc
    | hd :: tail ->
      let galaxies =
        List.foldi hd ~init:[] ~f:(fun x acc c ->
          match c with
          | '.' -> acc
          | '#' -> (x, y) :: acc
          | _ -> failwith "Invalid character!")
      in
      aux (y + 1, galaxies @ acc) tail
  in
  aux (0, []) universe
;;

let fix factor galaxies =
  List.map galaxies ~f:(fun (x, y) ->
    let x' =
      let correction = List.count xs_to_expand ~f:(fun x' -> x' < x) in
      x + (correction * (factor - 1))
    in
    let y' =
      let correction = List.count ys_to_expand ~f:(fun y' -> y' < y) in
      y + (correction * (factor - 1))
    in
    x', y')
;;

let manhattan_distance (xa, ya) (xb, yb) = abs (xb - xa) + abs (yb - ya)

let generate_pairs list =
  let rec aux acc list =
    match list with
    | hd :: tail ->
      let newpairs =
        List.fold list ~init:[] ~f:(fun acc elt ->
          if fst hd = fst elt && snd hd = snd elt then acc else (hd, elt) :: acc)
      in
      aux (newpairs @ acc) tail
    | [] -> acc
  in
  aux [] list
;;

let () = printf "%d\n" (manhattan_distance (1, 6) (5, 11))
let galaxies = parse_data data |> List.rev |> fix 2

let result =
  generate_pairs galaxies
  |> List.map ~f:(fun (a, b) -> manhattan_distance a b)
  |> List.fold ~init:0 ~f:( + )
;;

let () = printf "Part1: %d\n" result
let galaxies = parse_data data |> List.rev |> fix 1000000

let result =
  generate_pairs galaxies
  |> List.map ~f:(fun (a, b) -> manhattan_distance a b)
  |> List.fold ~init:0 ~f:( + )
;;

let () = printf "Part2: %d\n" result
