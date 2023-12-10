open Blackjack

(* module SpotCardGenerator = Card.SpotCard module FaceCardGenerator =
   Card.FaceCard module PersonGenerator = Person.Person_Gen *)
module GameGenerator = Gameone.Fullgamegen

let _ = Random.self_init ()

(* let generate_card rand_num rand_suit = let suit = match rand_suit with | 0 ->
   SpotCardGenerator.Clubs | 1 -> SpotCardGenerator.Diamonds | 2 ->
   SpotCardGenerator.Hearts | 3 -> SpotCardGenerator.Spades | _ -> failwith
   "Invalid suit" in SpotCardGenerator.print (SpotCardGenerator.create rand_num
   suit) (** This specific function makes a player who takes in function input
   to decide wheter they want to keep gettng cards or not*) let rec repl
   player_hand : int = print_string "> "; let input = read_line () in match
   input with | "H" -> print_endline "You chose to hit"; let rand_num = 2 +
   Random.int 8 in let rand_suit = Random.int 4 in generate_card rand_num
   rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 player_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "You busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "You got 21!"; 21) else (
   print_endline ("Your hand's current value is " ^ string_of_int
   player_hand_sum); repl (rand_num :: player_hand)) | "S" -> print_endline "You
   chose to stay"; List.fold_left ( + ) 0 player_hand | _ -> print_endline
   "Thanks for playing!"; List.fold_left ( + ) 0 player_hand (** This
   representgs a general cpu who is hardcoded to stop sasking for cards when
   their total card sum passes 17. They represent a more aggresive player*) (*
   let rec agg_repl_cpu cpu_hand = print_string "> "; let cpu_sum =
   List.fold_left ( + ) 0 cpu_hand in

   let input = cpu_sum < 17 in match input with | true -> print_endline "You
   chose to hit"; let rand_num = 2 + Random.int 8 in let rand_suit = Random.int
   4 in generate_card rand_num rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "You busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "You win!"; 21) else (
   print_endline ("Your hand's current value is " ^ string_of_int
   player_hand_sum); agg_repl_cpu (rand_num :: cpu_hand)) | false ->
   print_endline "You chose to stay"; cpu_sum *)

   (** This represents a more passive player who stops playign when their total
   hand surpasses 15. *) let rec repl_cpu cpu_hand = print_string "> "; let
   cpu_sum = List.fold_left ( + ) 0 cpu_hand in

   let input = cpu_sum < 15 in match input with | true -> print_endline "CPU
   chose to hit"; let rand_num = 2 + Random.int 8 in let rand_suit = Random.int
   4 in generate_card rand_num rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "CPU busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "CPU won!"; 21) else (
   print_endline ("CPU hand's current value is " ^ string_of_int
   player_hand_sum); repl_cpu (rand_num :: cpu_hand)) | false -> print_endline
   "CPU chose to stay"; cpu_sum

   (** This representgs a general cpu who is hardcoded to stop sasking for cards
   when their total card sum passes 17. They represent a more aggresive player*)
   let rec aggresive_repl_cpu cpu_hand = print_string "> "; let cpu_sum =
   List.fold_left ( + ) 0 cpu_hand in

   let input = cpu_sum < 17 in match input with | true -> print_endline "CPU
   chose to hit"; let rand_num = 2 + Random.int 8 in let rand_suit = Random.int
   4 in generate_card rand_num rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "CPU busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "CPU won!"; 21) else (
   print_endline ("CPU hand's current value is " ^ string_of_int
   player_hand_sum); aggresive_repl_cpu (rand_num :: cpu_hand)) | false ->
   print_endline "CPU chose to stay"; cpu_sum

   let rec decide_2 first second currenthand = let failed = ref 0 in if first =
   0 then failed := !failed + 1 else failed := !failed; if second = 0 then
   failed := !failed + 1 else failed := !failed; if !failed > 1 then 15 else let
   max_total = max first second in if max_total > 20 then 15 else if max_total <
   15 then 15 else max_total (** This represents a player who uses the outputs
   of the first two players to determine wheterer they still want to get more
   cards*) let rec third_input_cpu first second cpu_hand = print_string "> ";
   let cpu_sum = List.fold_left ( + ) 0 cpu_hand in let decide_num = decide_2
   first second cpu_sum in let input = cpu_sum < decide_num in match input with
   | true -> print_endline "CPU chose to hit"; let rand_num = 2 + Random.int 8
   in let rand_suit = Random.int 4 in generate_card rand_num rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "CPU busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "CPU won!"; 21) else (
   print_endline ("CPU hand's current value is " ^ string_of_int
   player_hand_sum); third_input_cpu first second (rand_num :: cpu_hand)) |
   false -> print_endline "CPU chose to stay"; cpu_sum let rec decide_3 first
   second third currenthand = let failed = ref 0 in if first = 0 then failed :=
   !failed + 1 else failed := !failed; if second = 0 then failed := !failed + 1
   else failed := !failed; if third = 0 then failed := !failed + 1 else failed
   := !failed;

   if !failed > 2 then 13 else let max_total1 = max first second in let
   max_total = max max_total1 third in if max_total > 20 then 15 else if
   max_total < 15 then 15 else max_total

   (** This represents a player who uses the outputs of the first three players
   to determine wheterer they still want to get more cards*) let rec
   fourth_input_cpu first second third cpu_hand = print_string "> "; let cpu_sum
   = List.fold_left ( + ) 0 cpu_hand in let decide_num = decide_3 first second
   third cpu_sum in let input = cpu_sum < decide_num in match input with | true
   -> print_endline "CPU chose to hit"; let rand_num = 2 + Random.int 8 in let
   rand_suit = Random.int 4 in generate_card rand_num rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "CPU busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "CPU won!"; 21) else (
   print_endline ("CPU hand's current value is " ^ string_of_int
   player_hand_sum); fourth_input_cpu first second third (rand_num :: cpu_hand))
   | false -> print_endline "CPU chose to stay"; cpu_sum

   let rec dealer_decide first second third fourth player dealer_val : bool =
   let failed = ref 0 in if first = 0 then failed := !failed + 1 else failed :=
   !failed; if second = 0 then failed := !failed + 1 else failed := !failed; if
   third = 0 then failed := !failed + 1 else failed := !failed; if fourth = 0
   then failed := !failed + 1 else failed := !failed; if player = 0 then failed
   := !failed + 1 else failed := !failed;

   if !failed > 4 then true else let count = ref 0 in if dealer_val > first then
   count := !count + 1 else count := !count; if dealer_val > second then failed
   := !count + 1 else count := !count; if dealer_val > third then count :=
   !count + 1 else count := !count; if dealer_val > fourth then count := !count
   + 1 else count := !count; if dealer_val > player then count := !count + 1
   else count := !count; if !count > 4 then false else if !count > 3 &&
   dealer_val > player then false else if !count > 2 && dealer_val > 15 then
   false else true (** Represents a dealer player who uses the outputs of all
   the other players to select the best choice of wheter they should still keep
   taking cards.*) let rec dealer_player first second third fourth player
   dealer_hand = print_string "> "; let dealer_sum = List.fold_left ( + ) 0
   dealer_hand in (* let decide_num = decide_3 first second third cpu_sum in *)
   let input : bool = dealer_decide first second third fourth player dealer_sum
   in match input with | true -> print_endline "Dealer chose to hit"; let
   rand_num = 2 + Random.int 8 in let rand_suit = Random.int 4 in generate_card
   rand_num rand_suit;

   let player_hand_sum = List.fold_left ( + ) 0 dealer_hand + rand_num in if
   player_hand_sum > 21 then ( print_endline "Dealer busted!"; 0) else if
   player_hand_sum = 21 then ( print_endline "Dealer won!"; 21) else (
   print_endline ("Dealer hand's current value is " ^ string_of_int
   player_hand_sum); dealer_player first second third fourth player (rand_num ::
   dealer_hand)) | false -> print_endline "Dealer chose to stay"; dealer_sum

   let winner name= print_endline(name ^ " is a winner");
   PersonGenerator.print_large_stick_figure_smile()

   let loser name= print_endline(name^ "Lost");
   PersonGenerator.print_large_stick_figure_nosmile() *)

let () =
  print_endline "\n\nWelcome to Blackjack!\n";
  let frst_cpu = GameGenerator.repl_cpu [] 15 in
  print_endline ("First cpu score " ^ string_of_int frst_cpu);
  let second_cpu = GameGenerator.repl_cpu [] 17 in
  print_endline ("Second cpu score " ^ string_of_int second_cpu);
  let third_cpu = GameGenerator.third_input_cpu frst_cpu second_cpu [] in
  print_endline ("Third cpu score " ^ string_of_int third_cpu);
  let fourth_cpu =
    GameGenerator.fourth_input_cpu frst_cpu second_cpu third_cpu []
  in
  print_endline ("Fourth cpu score " ^ string_of_int fourth_cpu);
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline
    "Now it is time for you to play the game. You can scroll up to see the \
     cpu's score. After you player the dealer will play and you will see if \
     you win";
  print_endline "Press H to hit, S to stay, or E to exit";
  let player_val = GameGenerator.repl [] in
  print_endline ("Player score " ^ string_of_int player_val);
  let dealer_val =
    GameGenerator.dealer_player frst_cpu second_cpu third_cpu fourth_cpu
      player_val []
  in
  print_endline "";
  print_endline "";
  print_endline ("The Dealers FInal Score was :" ^ string_of_int dealer_val);

  if frst_cpu > dealer_val then GameGenerator.winner "First CPU"
  else GameGenerator.loser "First CPU";
  print_endline "";
  print_endline "";
  if second_cpu > dealer_val then GameGenerator.winner "Second CPU"
  else GameGenerator.loser "Second CPU";
  print_endline "";
  print_endline "";
  if third_cpu > dealer_val then GameGenerator.winner "Third CPU"
  else GameGenerator.loser "Third  CPU";
  print_endline "";
  print_endline "";
  if fourth_cpu > dealer_val then GameGenerator.winner "Fourth CPU"
  else GameGenerator.loser "Fourth CPU";
  print_endline "";
  print_endline "";
  if player_val > dealer_val then GameGenerator.winner "Player"
  else GameGenerator.loser "Player"
