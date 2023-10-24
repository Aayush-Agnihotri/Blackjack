open Blackjack
module SpotCardGenerator = Card.SpotCard
module FaceCardGenerator = Card.FaceCard

let _ = Random.self_init ()

let generate_card rand_num rand_suit =
  let suit =
    match rand_suit with
    | 0 -> SpotCardGenerator.Clubs
    | 1 -> SpotCardGenerator.Diamonds
    | 2 -> SpotCardGenerator.Hearts
    | 3 -> SpotCardGenerator.Spades
    | _ -> failwith "Invalid suit"
  in
  SpotCardGenerator.print (SpotCardGenerator.create rand_num suit)

let rec repl player_hand =
  print_string "> ";
  let input = read_line () in
  match input with
  | "H" ->
      print_endline "You chose to hit";
      let rand_num = 2 + Random.int 8 in
      let rand_suit = Random.int 4 in
      generate_card rand_num rand_suit;

      let player_hand_sum = List.fold_left ( + ) 0 player_hand + rand_num in
      if player_hand_sum > 21 then print_endline "You busted!"
      else if player_hand_sum = 21 then print_endline "You win!"
      else (
        print_endline
          ("Your hand's current value is " ^ string_of_int player_hand_sum);
        repl (rand_num :: player_hand))
  | "S" ->
      print_endline "You chose to stay";
      repl player_hand
  | _ -> print_endline "Thanks for playing!"

let () =
  print_endline "\n\nWelcome to Blackjack!\n";
  print_endline "Press H to hit, S to stay, or E to exit";
  repl []
