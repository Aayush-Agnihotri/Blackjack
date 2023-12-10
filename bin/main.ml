open Blackjack

(* module SpotCardGenerator = Card.SpotCard module FaceCardGenerator =
   Card.FaceCard module PersonGenerator = Person.Person_Gen *)
module GameGenerator = Gameone.Fullgamegen

let _ = Random.self_init ()

let () =
  print_endline "\n\nWelcome to Blackjack!\n";
  print_endline
    "How much money do you want Cpus to  bet (Only Enter a number): ";
  let cpu_bet = read_int () in
  (* let weight = match cpu_bet with | x -> x | _ -> failwith "error" in *)
  print_endline "How much money do you want to bet (Only enter a number ): ";
  let play_bet = read_int () in
  let weightp =
    match play_bet with
    | x -> x
    (* | _ -> failwith "error" *)
  in
  let paly_bet1 = weightp in

  print_endline
    "The game is going to start now. You will be playing with 4 randomly \
     generated CPUS. Your goal is to draw enough cards to get as close to 21 \
     as possible without going over. Type Ok and enter to start the game.";
  let input = read_line () in
  print_endline input;
  let frst_cpu = GameGenerator.repl_cpu [] 15 in
  print_endline ("First cpu score " ^ string_of_int frst_cpu);
  print_endline
    "The first cpu just finished you can scroll up to see thier score. Enter \
     ok to continue";
  let in1 = read_line () in
  print_endline in1;
  let second_cpu = GameGenerator.repl_cpu [] 17 in
  print_endline ("Second cpu score " ^ string_of_int second_cpu);
  print_endline
    "The second cpu just finished you can scroll up to see thier score. Enter \
     ok to continue";
  let in2 = read_line () in
  print_endline in2;
  let third_cpu = GameGenerator.third_input_cpu frst_cpu second_cpu [] in
  print_endline ("Third cpu score " ^ string_of_int third_cpu);
  print_endline
    "The third cpu just finished you can scroll to see thier score. Enter ok \
     to continue";
  let in3 = read_line () in
  print_endline in3;
  let fourth_cpu =
    GameGenerator.fourth_input_cpu frst_cpu second_cpu third_cpu []
  in
  print_endline ("Fourth cpu score " ^ string_of_int fourth_cpu);
  print_endline
    "The fourth cpu just finished you can scroll up to see thier score. Enter \
     ok to continue";
  let in4 = read_line () in
  print_endline in4;
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline
    "Before you start playing would you like to add more money to your bet. \
     Enter the number of how much more yoy would like to add";
  let morebet = read_int () in
  let weightp =
    match morebet with
    | x -> x
    (* | _ -> failwith "error" *)
  in
  let finalbet = weightp + paly_bet1 in

  print_endline "Your turn:";
  print_endline "Press H to hit, S to stay, or E to exit";
  let player_val = GameGenerator.repl [] in
  print_endline
    (" Your final score was: " ^ string_of_int player_val
   ^ " Enter ok to go to dealer results");
  let in1 = read_line () in
  print_endline in1;
  let dealer_val =
    GameGenerator.dealer_player frst_cpu second_cpu third_cpu fourth_cpu
      player_val []
  in
  print_endline "";
  print_endline "";
  print_endline ("The Dealers Final Score was:" ^ string_of_int dealer_val);
  print_endline
    "The dealer just finished you can scroll to see thier score. Enter ok to \
     see the final results";
  let in5 = read_line () in
  print_endline in5;
  if frst_cpu > dealer_val then
    GameGenerator.winner false "First CPU" (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false "First CPU";
  print_endline
    "The first's cpu's results are being shown. You can scroll up to see if \
     they won or lost. \n\
    \    Enter ok to see CPU2's results";
  let r1 = read_line () in
  print_endline r1;
  print_endline "";
  print_endline "";
  if second_cpu > dealer_val then
    GameGenerator.winner false "Second CPU" (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false "Second CPU";
  print_endline
    "The second cpu's results are being shown. You can scroll up to see if \
     they won or lost. \n\
    \    Enter ok to see CPU3's results";
  let r2 = read_line () in
  print_endline r2;
  print_endline "";
  print_endline "";
  if third_cpu > dealer_val then
    GameGenerator.winner false "Third CPU" (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false "Third  CPU";
  print_endline
    "The third cpu's results are being shown. You can scroll up to see if they \
     won or lost. \n\
    \    Enter ok to see CPU4's results";
  let r3 = read_line () in
  print_endline r3;
  print_endline "";
  print_endline "";
  if fourth_cpu > dealer_val then
    GameGenerator.winner false "Fourth CPU" (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false "Fourth CPU";
  print_endline
    "The fourth cpu's results are being shown. You can scroll up to see if \
     they won or lost. \n\
    \    Enter ok to see YOUR results";
  let r4 = read_line () in
  print_endline r4;
  print_endline "";
  print_endline "";
  if player_val > dealer_val then
    GameGenerator.winner true "Player" (string_of_int (finalbet * 2))
  else GameGenerator.loser true "Player";
  print_endline
    "Your results are being shown you can scroll up to see how you fared"
