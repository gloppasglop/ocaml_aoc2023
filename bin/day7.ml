open Base
open Stdio

let file = "day7.txt"
let data = In_channel.read_lines file

module Card = struct
  type t =
    | Deuce
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
  [@@deriving compare, sexp_of, hash]

  let card_strength = function
    | Deuce -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14
  ;;

  let card_strength_jack = function
    | Deuce -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 1
    | Queen -> 12
    | King -> 13
    | Ace -> 14
  ;;

  let to_string = function
    | Deuce -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "T"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  ;;

  let compare card1 card2 = Int.compare (card_strength card1) (card_strength card2)

  let compare_jack card1 card2 =
    Int.compare (card_strength_jack card1) (card_strength_jack card2)
  ;;

  let equal card1 card2 = Int.equal (card_strength card1) (card_strength card2)

  let of_char = function
    | 'A' -> Ace
    | '2' -> Deuce
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | _ -> failwith "Not a valid car d"
  ;;
end

type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

let handtype_strength = function
  | FiveOfAKind -> 70
  | FourOfAKind -> 60
  | FullHouse -> 50
  | ThreeOfAKind -> 40
  | TwoPair -> 30
  | OnePair -> 20
  | HighCard -> 10
;;

let hand_type hand =
  let h = Hashtbl.create (module Card) in
  List.iter hand ~f:(fun card ->
    let _ = Hashtbl.incr ~by:1 h card in
    ());
  let comparison = List.sort (Hashtbl.data h) ~compare:Int.compare in
  let result =
    match comparison with
    | [ 5 ] -> FiveOfAKind
    | [ 1; 4 ] -> FourOfAKind
    | [ 2; 3 ] -> FullHouse
    | [ 1; 1; 3 ] -> ThreeOfAKind
    | [ 1; 2; 2 ] -> TwoPair
    | [ 1; 1; 1; 2 ] -> OnePair
    | [ 1; 1; 1; 1; 1 ] -> HighCard
    | _ -> failwith "hand_type Impossible"
  in
  result
;;

let _hand_to_string hand =
  List.fold hand ~init:"" ~f:(fun acc card -> acc ^ Card.to_string card)
;;

let compare_hand_type hand1 hand2 =
  let hand1_strength = hand_type hand1 |> handtype_strength in
  let hand2_strength = hand_type hand2 |> handtype_strength in
  let compare_hand_type = Int.compare hand1_strength hand2_strength in
  compare_hand_type
;;

let compare_hand_card hand1 hand2 =
  let cmp = List.map2_exn hand1 hand2 ~f:(fun card1 card2 -> Card.compare card1 card2) in
  match List.filter cmp ~f:(fun n -> n <> 0) with
  | [] -> 0
  | hd :: _ -> hd
;;

let compare_hand_card_jack hand1 hand2 =
  let cmp =
    List.map2_exn hand1 hand2 ~f:(fun card1 card2 -> Card.compare_jack card1 card2)
  in
  match List.filter cmp ~f:(fun n -> n <> 0) with
  | [] -> 0
  | hd :: _ -> hd
;;

let compare_hand hand1 hand2 =
  let compare_hand_type = compare_hand_type hand1 hand2 in
  if compare_hand_type = 0 then compare_hand_card hand1 hand2 else compare_hand_type
;;

let parse_hand str = List.map (String.to_list str) ~f:Card.of_char

let parse_line str =
  let hand, bid =
    match String.split str ~on:' ' with
    | [ hand; bid ] -> hand, Int.of_string bid
    | _ -> failwith "Fail to parse"
  in
  parse_hand hand, bid
;;

let parse data = List.map data ~f:parse_line
let parsed_data = parse data

let () =
  let sorted_hands =
    List.sort parsed_data ~compare:(fun (hand1, _) (hand2, _) -> compare_hand hand1 hand2)
  in
  let result =
    List.foldi sorted_hands ~init:0 ~f:(fun i acc (_, bid) -> acc + ((i + 1) * bid))
  in
  printf "Part1:  %d\n" result
;;

let replace_joker hand =
  let all_cards =
    Card.[ Ace; Deuce; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Queen; King ]
  in
  let newhands =
    List.fold hand ~init:[ [] ] ~f:(fun acc card ->
      if Card.equal card Card.Jack
      then
        List.concat
          (List.map acc ~f:(fun h ->
             List.map all_cards ~f:(fun replacement_card -> replacement_card :: h)))
      else List.map acc ~f:(fun h -> card :: h))
  in
  List.map newhands ~f:List.rev
;;

(*
   let hand = Card.[ Jack; Jack; Jack; Jack; Jack ]

   let () =
   List.iter (replace_joker hand) ~f:(fun hand ->
   List.iter (List.rev hand) ~f:(fun card -> printf " %d" (Card.card_strength card));
   print_endline "---")
   ;;
*)

let replaced_hands =
  List.map parsed_data ~f:(fun (hand, bid) ->
    let newhands = replace_joker hand in
    (* get strongest hand*)
    let strongest_hand =
      List.hd_exn
        (List.sort newhands ~compare:(fun hand1 hand2 -> compare_hand hand2 hand1))
    in
    (hand, bid), strongest_hand)
;;

let () =
  let sorted_hands =
    List.sort
      replaced_hands
      ~compare:
        (fun
          ((original_hand1, _), strongest_hand1) ((original_hand2, _), strongest_hand2) ->
        let compared_strongest = compare_hand_type strongest_hand1 strongest_hand2 in
        if compared_strongest = 0
        then compare_hand_card_jack original_hand1 original_hand2
        else compared_strongest)
  in
  let result =
    List.foldi sorted_hands ~init:0 ~f:(fun i acc ((_, bid), _) -> acc + ((i + 1) * bid))
  in
  printf "Part2: %d\n" result
;;
