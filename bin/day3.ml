open Base
open Stdio

let file = "day3.txt"
let data = In_channel.read_lines file

let get_numbers i s =
  let lc = String.to_list s in
  List.fold lc ~init:(true, 0, [], []) ~f:(fun acc c ->
    let new_number, pos, numbers, symbols = acc in
    if Char.is_digit c
    then
      if new_number
      then false, pos + 1, (pos, i, String.of_char c) :: numbers, symbols
      else
        ( false
        , pos + 1
        , (match numbers with
           | (pos, i, x) :: xs -> (pos, i, x ^ String.of_char c) :: xs
           | [] -> [ pos, i, String.of_char c ])
        , symbols )
    else if Char.equal c '.'
    then true, pos + 1, numbers, symbols
    else
      ( true
      , pos + 1
      , numbers
      , match symbols with
        | [] -> [ pos, i, c ]
        | _ -> (pos, i, c) :: symbols ))
;;

(*
   let () =
   List.iteri data ~f:(fun i line ->
   let _, _, numbers, symbols = get_numbers i line in
   List.iter numbers ~f:(fun (x, y, number) -> printf "%s(%d,%d) " number x y);
   List.iter symbols ~f:(fun (x, y, symbol) -> printf "%c(%d,%d) " symbol x y);
   print_endline "")
   ;;
*)

let intersect symbol number =
  let symbol_x, symbol_y, _ = symbol in
  let n_x, n_y, n = number in
  let top_left_x, top_left_y = n_x - 1, n_y - 1 in
  let bottom_right_x, bottom_right_y = n_x + String.length n, n_y + 1 in
  (symbol_x >= top_left_x && symbol_x <= bottom_right_x)
  && symbol_y >= top_left_y
  && symbol_y <= bottom_right_y
;;

let intersect_symbols symbols number =
  let res = List.filter symbols ~f:(fun symbol -> intersect symbol number) in
  List.length res > 0
;;

let filter_adjacent_numbers numbers symbols =
  List.filter numbers ~f:(intersect_symbols symbols)
;;

let numbers, symbols =
  List.foldi data ~init:([], []) ~f:(fun i (numbers, symbols) line ->
    let _, _, ns, ss = get_numbers i line in
    numbers @ ns, symbols @ ss)
;;

let () =
  let adjacent_numbers = filter_adjacent_numbers numbers symbols in
  let result =
    List.fold adjacent_numbers ~init:0 ~f:(fun acc (_, _, number) ->
      acc + Int.of_string number)
  in
  printf "Part1: %d\n" result
;;

let star_symbols = List.filter symbols ~f:(fun (_, _, symbol) -> Char.equal symbol '*')

let () =
  let intersect_star =
    List.map star_symbols ~f:(fun symbol -> List.filter numbers ~f:(intersect symbol))
  in
  let intersect_start_two =
    List.filter intersect_star ~f:(fun numbers -> List.length numbers = 2)
  in
  let result =
    List.fold intersect_start_two ~init:0 ~f:(fun acc numbers ->
      let product =
        List.fold numbers ~init:1 ~f:(fun acc (_, _, number) ->
          acc * Int.of_string number)
      in
      acc + product)
  in
  printf "Part2: %d\n" result
;;
