open Base
open Stdio

let file = "day2.txt"
let data = In_channel.read_lines file

type color =
  | RED
  | GREEN
  | BLUE

type draw = int * color

type game =
  { id : int
  ; draws : draw list list
  }

let color_of_string s =
  if String.equal s "red" then RED else if String.equal s "blue" then BLUE else GREEN
;;

let draw_of_string s : draw =
  let tmp =
    List.filter (String.split ~on:' ' s) ~f:(fun str -> not (String.equal str ""))
  in
  match tmp with
  | [] -> failwith "Impossible"
  | x :: xs ->
    ( Int.of_string x
    , let c =
        match xs with
        | [] -> failwith "impossoble"
        | y :: _ -> y
      in
      color_of_string c )
;;

let parse_game game =
  let tmp = String.split_on_chars ~on:[ ':'; ';' ] game in
  let draws =
    match tmp with
    | [] -> failwith "Not possible"
    | _ :: tail -> tail
  in
  List.map draws ~f:(fun draw ->
    let res = String.split ~on:',' draw in
    let cubes = List.map res ~f:draw_of_string in
    cubes)
;;

let _string_of_color = function
  | RED -> "red"
  | GREEN -> "green"
  | BLUE -> "blue"
;;

let is_valid_game draws =
  let valid_draws =
    List.filter draws ~f:(fun draw ->
      List.fold draw ~init:true ~f:(fun acc (count, color) ->
        acc
        &&
        match color with
        | RED -> count <= 12
        | GREEN -> count <= 13
        | BLUE -> count <= 14))
  in
  List.length valid_draws = List.length draws
;;

let parsed_games = List.mapi data ~f:(fun i line -> { id = i; draws = parse_game line })
let valid_games = List.filter parsed_games ~f:(fun game -> is_valid_game game.draws)
let result = List.fold valid_games ~init:0 ~f:(fun acc game -> acc + game.id + 1)
let () = printf "Part1: %d\n" result

let min_cubes draws =
  List.fold draws ~init:(0, 0, 0) ~f:(fun acc draw ->
    List.fold draw ~init:acc ~f:(fun acc' (count, color) ->
      let r', g', b' = acc' in
      match color with
      | RED -> max count r', g', b'
      | GREEN -> r', max count g', b'
      | BLUE -> r', g', max count b'))
;;

let () =
  let mcs =
    List.map parsed_games ~f:(fun game ->
      let r, g, b = min_cubes game.draws in
      r * g * b)
  in
  printf "Part2: %d\n" (List.fold mcs ~init:0 ~f:( + ))
;;
