open Base
open Stdio

let file = "day4.txt"
let data = In_channel.read_lines file

let parse_card_id str =
  match List.filter (String.split ~on:' ' str) ~f:(fun l -> not (String.equal "" l)) with
  | _ :: card_id :: _ -> card_id
  | _ -> failwith "Impossible"
;;

let parse_card_numbers str =
  List.filter (String.split ~on:' ' str) ~f:(fun l -> not (String.equal "" l))
;;

let parse_card_line str =
  match String.split ~on:'|' str with
  | winning_numbers :: played_numbers :: _ ->
    parse_card_numbers winning_numbers, parse_card_numbers played_numbers
  | _ -> failwith "Impossible"
;;

let parse_line line =
  let tmp = String.split ~on:':' line in
  let card_id, (winning, played) =
    match tmp with
    | x :: rest :: _ -> parse_card_id x, parse_card_line rest
    | _ -> failwith "Impossible"
  in
  card_id, winning, played
;;

let is_winning winning_numbers number =
  List.mem winning_numbers number ~equal:String.( = )
;;

let played_winning_numbers winning_numbers played_numbers =
  List.filter played_numbers ~f:(is_winning winning_numbers)
;;

let () =
  let draw_points =
    List.map data ~f:(fun line ->
      let _, winning_numbers, played_numbers = parse_line line in
      let winning_count =
        List.length (played_winning_numbers winning_numbers played_numbers)
      in
      let points = if winning_count > 0 then 1 lsl (winning_count - 1) else 0 in
      points)
  in
  printf "Part1 : %d\n" (List.fold draw_points ~init:0 ~f:( + ))
;;

let parse_games data = List.map data ~f:parse_line
let parsed_games = parse_games data
let cards = Hashtbl.create (module Int)

let () =
  List.iter parsed_games ~f:(fun (card_id, _, _) ->
    let _ = Hashtbl.add cards ~key:(Int.of_string card_id) ~data:1 in
    ())
;;

let () =
  List.iter parsed_games ~f:(fun (card_id, winning_numbers, played_numbers) ->
    let card_id = Int.of_string card_id in
    let winning_count =
      List.length (played_winning_numbers winning_numbers played_numbers)
    in
    if winning_count > 0
    then (
      let card_count =
        match Hashtbl.find cards card_id with
        | Some n -> n
        | None -> failwith "Imposssible"
      in
      let () = Hashtbl.set cards ~key:card_id ~data:card_count in
      for i = 1 to winning_count do
        let previous_count =
          match Hashtbl.find cards (card_id + i) with
          | Some n -> n
          | None -> failwith "Impossible"
        in
        let () =
          Hashtbl.set cards ~key:(card_id + i) ~data:(card_count + previous_count)
        in
        ()
      done)
    else ());
  Hashtbl.fold cards ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
  |> printf "Part2: %d\n"
;;
