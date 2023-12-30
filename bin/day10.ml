open Base
open Stdio

module Pipe = struct
  type t =
    | Ground
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthEast
    | SouthWest
    | Start
    | Inside

  let of_char = function
    | '.' -> Ground
    | '-' -> EastWest
    | '|' -> NorthSouth
    | 'L' -> NorthEast
    | 'J' -> NorthWest
    | '7' -> SouthWest
    | 'F' -> SouthEast
    | 'S' -> Start
    | 'I' -> Inside
    | _ -> Ground
  ;;

  (*
     let to_char = function
     | Ground -> '.'
     | EastWest -> '-'
     | NorthSouth -> '|'
     | NorthEast -> 'L'
     | NorthWest -> 'J'
     | SouthWest -> '7'
     | SouthEast -> 'F'
     | Start -> 'S'
     | Inside -> 'I'
     ;;
  *)

  let valid_neighbours = function
    | Ground -> [], [], [], []
    | Inside -> [], [], [], []
    | EastWest ->
      [], [ NorthWest; SouthWest; EastWest ], [], [ NorthEast; SouthEast; EastWest ]
    | NorthSouth ->
      [ NorthSouth; SouthEast; SouthWest ], [], [ NorthSouth; NorthEast; NorthWest ], []
    | NorthEast ->
      [ NorthSouth; SouthEast; SouthWest ], [ NorthWest; EastWest; SouthWest ], [], []
    | NorthWest ->
      [ NorthSouth; SouthEast; SouthWest ], [], [], [ EastWest; SouthEast; NorthEast ]
    | SouthEast ->
      [], [ EastWest; SouthWest; NorthWest ], [ NorthSouth; NorthWest; NorthEast ], []
    | SouthWest ->
      [], [], [ NorthSouth; NorthEast; NorthWest ], [ EastWest; NorthEast; SouthEast ]
    | Start ->
      ( [ NorthSouth; NorthEast; NorthWest; EastWest; SouthEast; SouthWest ]
      , [ NorthSouth; NorthEast; NorthWest; EastWest; SouthEast; SouthWest ]
      , [ NorthSouth; NorthEast; NorthWest; EastWest; SouthEast; SouthWest ]
      , [ NorthSouth; NorthEast; NorthWest; EastWest; SouthEast; SouthWest ] )
  ;;

  let equal p1 p2 =
    match p1, p2 with
    | Ground, Ground -> true
    | EastWest, EastWest -> true
    | NorthSouth, NorthSouth -> true
    | NorthEast, NorthEast -> true
    | NorthWest, NorthWest -> true
    | SouthWest, SouthWest -> true
    | SouthEast, SouthEast -> true
    | Start, Start -> true
    | _, _ -> false
  ;;
end

let file = "day10.txt"
let data = In_channel.read_lines file
let width = String.length (List.hd_exn data)
let height = List.length data

type cell =
  { pipe : Pipe.t
  ; x : int
  ; y : int
  }

let cells = Array.make_matrix ~dimx:height ~dimy:width { pipe = Ground; x = 0; y = 0 }

let () =
  List.iteri data ~f:(fun y line ->
    cells.(y)
    <- Array.mapi (String.to_array line) ~f:(fun x cell ->
         { pipe = Pipe.of_char cell; x; y }))
;;

let get_cell arr x y =
  try Some arr.(y).(x) with
  | _ -> None
;;

let connected_neighbours cells cell =
  match cell with
  | None -> None, None, None, None
  | Some { pipe; x; y } ->
    let north = get_cell cells x (y - 1) in
    let south = get_cell cells x (y + 1) in
    let east = get_cell cells (x + 1) y in
    let west = get_cell cells (x - 1) y in
    let n, e, s, w = Pipe.valid_neighbours pipe in
    let north_match =
      match north with
      | None -> None
      | Some { pipe; x = _; y = _ } ->
        if List.mem n pipe ~equal:Pipe.equal then north else None
    in
    let south_match =
      match south with
      | None -> None
      | Some { pipe; x = _; y = _ } ->
        if List.mem s pipe ~equal:Pipe.equal then south else None
    in
    let east_match =
      match east with
      | None -> None
      | Some { pipe; x = _; y = _ } ->
        if List.mem e pipe ~equal:Pipe.equal then east else None
    in
    let west_match =
      match west with
      | None -> None
      | Some { pipe; x = _; y = _ } ->
        if List.mem w pipe ~equal:Pipe.equal then west else None
    in
    north_match, east_match, south_match, west_match
;;

let find_start cells =
  Array.fold cells ~init:None ~f:(fun accy line ->
    match accy with
    | Some _ -> accy
    | None ->
      Array.fold line ~init:accy ~f:(fun accx cell ->
        match accx with
        | Some _ -> accx
        | None ->
          (match cell.pipe with
           | Pipe.Start -> Some cell
           | _ -> accx)))
;;

let startpos =
  match find_start cells with
  | None -> failwith "Not found"
  | Some { pipe = _; x; y } -> x, y
;;

let startpipe =
  let x, y = startpos in
  (*
     let adjacent_cells = [(x-1,y);(x+1,y);(x,y-1);(x,1+1)] in
     let adjacent_cells= List.filter adjacent_cells ~f:(fun (x,y) -> x >= 0 && y >= 0 && x <= width-1 && y <= height -1) in
  *)
  let all_pipes =
    Pipe.[ NorthSouth; NorthEast; NorthWest; SouthEast; SouthWest; EastWest ]
  in
  let valid_pipes =
    List.filter all_pipes ~f:(fun candidate_pipe ->
      match connected_neighbours cells (Some { pipe = candidate_pipe; x; y }) with
      | Some _, Some _, None, None -> true
      | Some _, None, Some _, None -> true
      | Some _, None, None, Some _ -> true
      | None, Some _, Some _, None -> true
      | None, Some _, None, Some _ -> true
      | None, None, Some _, Some _ -> true
      | _ -> false)
  in
  valid_pipes
;;

(* replace start with startpipe*)
let startcell = { pipe = List.hd_exn startpipe; x = fst startpos; y = snd startpos }

let () =
  let line = cells.(startcell.y) in
  line.(startcell.x) <- startcell
;;

let walk cells startcell previouscell =
  let cell1, cell2 =
    match connected_neighbours cells (Some startcell) with
    | Some n, Some e, None, None -> n, e
    | Some n, None, Some s, None -> n, s
    | Some n, None, None, Some w -> n, w
    | None, Some e, Some s, None -> s, e
    | None, Some e, None, Some w -> w, e
    | None, None, Some s, Some w -> s, w
    | _ -> failwith "Impossible"
  in
  match previouscell with
  | Some previouscell ->
    if cell1.x = previouscell.x && cell1.y = previouscell.y then cell2 else cell1
  | None -> cell1
;;

let rec loop cells path startcell currentcell previouscell =
  let next_cell = walk cells currentcell previouscell in
  if next_cell.x = startcell.x && next_cell.y = startcell.y
  then path
  else loop cells (next_cell :: path) startcell next_cell (Some currentcell)
;;

let path = loop cells [ startcell ] startcell startcell None
let () = printf "Part1: %d\n" ((List.length path + 1) / 2)

(* Crete and empty cell array and only fill set the cells in the path*)
let () =
  List.iteri data ~f:(fun y line ->
    cells.(y)
    <- Array.mapi (String.to_array line) ~f:(fun x _ -> { pipe = Pipe.Ground; x; y }))
;;

let () = List.iter path ~f:(fun cell -> cells.(cell.y).(cell.x) <- cell)

(*
   Find the upper left cell
   it should be an F.
*)
let uppercell =
  List.fold
    path
    ~init:{ pipe = Pipe.Ground; x = Int.max_value; y = Int.max_value }
    ~f:(fun { pipe = accpipe; x = accx; y = accy } { pipe; x; y } ->
      if x <= accx && y <= accy
      then { pipe; x; y }
      else { pipe = accpipe; x = accx; y = accy })
;;

(* walk the path clockwise from the upper cell
   the walk function prioritize east direction*)
let path = loop cells [ uppercell ] uppercell uppercell None

(*
   Inside direction changes each time we turn
*)

type direction =
  | North
  | South
  | East
  | West

let inside_normal from pipe =
  match pipe, from with
  | Pipe.NorthEast, East -> North, East
  | Pipe.NorthEast, North -> South, West
  | Pipe.SouthEast, South -> South, East
  | Pipe.SouthEast, East -> North, West
  | Pipe.NorthWest, North -> North, West
  | Pipe.NorthWest, West -> South, East
  | Pipe.SouthWest, South -> North, East
  | Pipe.SouthWest, West -> South, West
  | Pipe.NorthSouth, North -> West, West
  | Pipe.NorthSouth, South -> East, East
  | Pipe.EastWest, East -> North, North
  | Pipe.EastWest, West -> South, South
  | _ -> failwith "Impossible"
;;

let colorize startpos direction =
  let startx, starty = startpos in
  let deltax, deltay =
    match direction with
    | North -> 0, -1
    | South -> 0, 1
    | East -> 1, 0
    | West -> -1, 0
  in
  let _ =
    List.fold
      (List.range 1 (Int.max height width))
      ~init:true
      ~f:(fun more i ->
        if more
        then (
          match get_cell cells (startx + (i * deltax)) (starty + (i * deltay)) with
          | None -> false
          | Some { pipe; x; y } ->
            (match pipe with
             | Pipe.Ground | Pipe.Inside ->
               cells.(y).(x) <- { pipe = Pipe.Inside; x; y };
               true
             | _ -> false))
        else more)
  in
  ()
;;

let rec colorize_inside path =
  match path with
  | current :: next :: rest ->
    let from =
      if next.x > current.x
      then West
      else if next.x < current.x
      then East
      else if next.y > current.y
      then North
      else South
    in
    let normal1, normal2 = inside_normal from next.pipe in
    colorize (next.x, next.y) normal1;
    colorize (next.x, next.y) normal2;
    colorize_inside (next :: rest)
  | _current :: [] -> ()
  | [] -> ()
;;

let () = colorize_inside path

let () =
  let result =
    Array.fold ~init:0 cells ~f:(fun acc line ->
      Array.fold line ~init:acc ~f:(fun acc cell ->
        match cell.pipe with
        | Pipe.Inside -> acc + 1
        | _ -> acc))
  in
  printf "Part2: %d\n" result
;;
