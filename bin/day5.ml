open Base
open Stdio

let file = "day5.txt"
let data = In_channel.read_lines file

type map =
  { source : int
  ; dest : int
  ; range : int
  }

type section =
  | Seeds
  | SeedToSoil
  | SoilToFertilizer
  | FertilizerToWater
  | WaterToLight
  | LightToTemperature
  | TemperatureToHumidity
  | HumiditytoLocation

type result =
  { seeds : int list
  ; s2s : map list
  ; s2f : map list
  ; f2w : map list
  ; w2l : map list
  ; l2t : map list
  ; t2h : map list
  ; h2l : map list
  }

let empty_result =
  { seeds = []; s2s = []; s2f = []; f2w = []; w2l = []; l2t = []; t2h = []; h2l = [] }
;;

(* Parse input *)

let string_to_intlist str = List.map (String.split str ~on:' ') ~f:Int.of_string

let string_to_map str =
  let tmp = String.split str ~on:' ' in
  match tmp with
  | [ dest; source; range ] ->
    { source = Int.of_string source
    ; dest = Int.of_string dest
    ; range = Int.of_string range
    }
  | _ -> failwith "Impossinle"
;;

let parse data =
  let rec parse_aux (current_section, result) data =
    match data with
    | [] -> current_section, result
    | head :: tail ->
      (match head with
       | "" -> parse_aux (current_section, result) tail
       | line ->
         (match String.chop_prefix line ~prefix:"seeds: " with
          | Some s -> parse_aux (Seeds, { result with seeds = string_to_intlist s }) tail
          | None ->
            (match line with
             | "seed-to-soil map:" -> parse_aux (SeedToSoil, result) tail
             | "soil-to-fertilizer map:" -> parse_aux (SoilToFertilizer, result) tail
             | "fertilizer-to-water map:" -> parse_aux (FertilizerToWater, result) tail
             | "water-to-light map:" -> parse_aux (WaterToLight, result) tail
             | "light-to-temperature map:" -> parse_aux (LightToTemperature, result) tail
             | "temperature-to-humidity map:" ->
               parse_aux (TemperatureToHumidity, result) tail
             | "humidity-to-location map:" -> parse_aux (HumiditytoLocation, result) tail
             | line ->
               (match current_section with
                | SeedToSoil ->
                  parse_aux
                    ( current_section
                    , { result with s2s = string_to_map line :: result.s2s } )
                    tail
                | SoilToFertilizer ->
                  parse_aux
                    ( current_section
                    , { result with s2f = string_to_map line :: result.s2f } )
                    tail
                | FertilizerToWater ->
                  parse_aux
                    ( current_section
                    , { result with f2w = string_to_map line :: result.f2w } )
                    tail
                | WaterToLight ->
                  parse_aux
                    ( current_section
                    , { result with w2l = string_to_map line :: result.w2l } )
                    tail
                | LightToTemperature ->
                  parse_aux
                    ( current_section
                    , { result with l2t = string_to_map line :: result.l2t } )
                    tail
                | TemperatureToHumidity ->
                  parse_aux
                    ( current_section
                    , { result with t2h = string_to_map line :: result.t2h } )
                    tail
                | HumiditytoLocation ->
                  parse_aux
                    ( current_section
                    , { result with h2l = string_to_map line :: result.h2l } )
                    tail
                | Seeds -> failwith "Impossibe"))))
  in
  parse_aux (Seeds, empty_result) data
;;

let parsed_data = parse data

let get_destination maps source =
  let potential_maps =
    List.filter maps ~f:(fun map ->
      source >= map.source && source < map.source + map.range)
  in
  let result =
    match List.length potential_maps with
    | 0 -> source
    | 1 ->
      let map = List.hd_exn potential_maps in
      source - map.source + map.dest
    | _ -> failwith "Impossible"
  in
  result
;;

let get_location instructions seed =
  get_destination instructions.s2s seed
  |> get_destination instructions.s2f
  |> get_destination instructions.f2w
  |> get_destination instructions.w2l
  |> get_destination instructions.l2t
  |> get_destination instructions.t2h
  |> get_destination instructions.h2l
;;

let () =
  let instructions = snd parsed_data in
  let seeds = instructions.seeds in
  let results = List.map seeds ~f:(fun seed -> get_location instructions seed) in
  printf " Part1: %d\n" (List.fold results ~init:Int.max_value ~f:min)
;;

(*
   Need to be smarter for part 2
   Brute force does not work
*)
let convert_seed_pairs seeds =
  let rec aux acc seeds =
    match seeds with
    | seed :: range :: rest -> (seed, range) :: aux acc rest
    | [] -> acc
    | _ -> failwith "Impossible convert seed"
  in
  aux [] seeds
;;

type interval =
  { min : int
  ; max : int
  }

let rec intersect i1 i2 =
  if i2.min < i1.min
  then intersect i2 i1
  else if i1.max < i2.min
  then None
  else if i2.max <= i1.max
  then Some i2
  else Some { min = i2.min; max = i1.max }
;;

let maps_to_intervals maps =
  let maps =
    List.sort maps ~compare:(fun map1 map2 -> Int.compare map1.source map2.source)
  in
  let first_map = List.hd_exn maps in
  let last_map = List.last_exn maps in
  let intervals =
    List.map maps ~f:(fun map ->
      map.dest - map.source, { min = map.source; max = map.source + map.range - 1 })
  in
  [ first_map.dest - first_map.source, { min = 0; max = max 0 (first_map.source - 1) } ]
  @ intervals
  @ [ 0, { min = last_map.source + last_map.range; max = Int.max_value } ]
;;

let _print_interval { min; max } = printf "[%d;%d]\n" min max

let step maps intervals =
  let map_intervals = maps_to_intervals maps in
  let result =
    List.map map_intervals ~f:(fun (offset, interval) ->
      List.map intervals ~f:(fun interval' ->
        match intersect interval interval' with
        | None -> None
        | Some { min; max } -> Some { min = min + offset; max = max + offset }))
    |> List.concat
    |> List.filter_opt
  in
  result
;;

let get_location_intervals instructions intervals =
  step instructions.s2s intervals
  |> step instructions.s2f
  |> step instructions.f2w
  |> step instructions.w2l
  |> step instructions.l2t
  |> step instructions.t2h
  |> step instructions.h2l
;;

let () =
  let instructions = snd parsed_data in
  let seeds = instructions.seeds in
  let seed_pairs = convert_seed_pairs seeds in
  let seed_intervals =
    List.map seed_pairs ~f:(fun (min, range) -> { min; max = min + range - 1 })
  in
  let result =
    get_location_intervals instructions seed_intervals
    |> List.sort ~compare:(fun i1 i2 -> Int.compare i1.min i2.min)
  in
  printf "Part2: %d\n" (List.hd_exn result).min
;;
