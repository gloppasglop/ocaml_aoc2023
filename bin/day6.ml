open Base
open Stdio
open Angstrom

let file = "day6.txt"
let data = In_channel.read_all file
let whitespace = take_while Char.is_whitespace
let integer = take_while1 Char.is_digit >>| Int.of_string <?> "Parser integer"
let eol = string "\n"

let time_parse =
  let* _ = string "Time:" in
  let* _ = skip_while Char.is_whitespace in
  let* times = sep_by1 whitespace integer in
  let* _ = eol in
  let* _ = string "Distance:" in
  let* _ = skip_while Char.is_whitespace in
  let* distances = sep_by1 whitespace integer in
  return (times, distances)
;;

let times, distances =
  match parse_string ~consume:Prefix time_parse data with
  | Ok (times, distances) -> times, distances
  | Error _ -> failwith "Error parsing"
;;

let solve t d =
  let delta = Float.of_int ((t * t) - (4 * d)) in
  let root1 = (Float.of_int t -. Float.sqrt delta) /. 2.0 in
  let root2 = (Float.of_int t +. Float.sqrt delta) /. 2.0 in
  root1, root2
;;

let roots' = List.map2 times distances ~f:(fun t d -> solve t d)

let roots =
  match roots' with
  | Ok roots -> roots
  | Unequal_lengths -> failwith "Impossible"
;;

let number_of_solutions (root1, root2) =
  let iroot1 = Float.round_up root1 in
  let iroot2 = Float.round_down root2 in
  if Float.(iroot1 = root1 && iroot2 = root2)
  then iroot2 -. iroot1 -. 1.0
  else iroot2 -. iroot1 +. 1.0
;;

let () =
  let sols = List.map roots ~f:number_of_solutions in
  let result = List.fold sols ~init:1.0 ~f:( *. ) in
  printf "Part1: %d\n" (Int.of_float result)
;;

let () =
  let time =
    Int.of_string
      (List.fold (List.rev times) ~init:"" ~f:(fun acc time -> Int.to_string time ^ acc))
  in
  let distance =
    Int.of_string
      (List.fold (List.rev distances) ~init:"" ~f:(fun acc distance ->
         Int.to_string distance ^ acc))
  in
  let result = solve time distance |> number_of_solutions |> Int.of_float in
  printf "Part2:  %d\n" result
;;
