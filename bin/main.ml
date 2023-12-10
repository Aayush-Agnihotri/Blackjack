open Blackjack
module GameGenerator = Gameone.Fullgamegen

let _ = Random.self_init ()
let system_color = "\027[32m"
let cpu1_color = "\027[31m"
let cpu2_color = "\027[33m"
let cpu3_color = "\027[36m"
let cpu4_color = "\027[35m"
let player_color = "\027[34m"

let () =
  print_endline (system_color ^ "\n\nWelcome to Blackjack!\n");
  print_endline
    (system_color
   ^ "How much money do you want CPUs to bet (enter a nonnegative integer)?");
  let cpu_bet =
    try read_int ()
    with Failure _ ->
      print_endline "Invalid input. Ending program.";
      exit 1
  in
  let cpu_bet =
    match cpu_bet with
    | x ->
        if x < 0 then (
          print_endline "Invalid input. Ending program.";
          exit 1)
        else x
  in
  print_endline
    (system_color
   ^ "How much money do you want to bet (enter a nonnegative integer)?");
  let play_bet =
    try read_int ()
    with Failure _ ->
      print_endline "Invalid input. Ending program.";
      exit 1
  in
  let play_bet =
    match play_bet with
    | x ->
        if x < 0 then (
          print_endline "Invalid input. Ending program.";
          exit 1)
        else x
  in
  let weightp =
    match play_bet with
    | x -> x
  in
  let paly_bet1 = weightp in

  print_endline
    (system_color
   ^ "The game will start now. You will be playing with 4 randomly generated \
      CPUS. Your goal is to draw enough cards to get as close to 21 as \
      possible without going over. Enter any key to start the game.");
  let _ = read_line () in
  print_endline ("\n" ^ cpu1_color ^ "Starting CPU1's turn");
  let frst_cpu = GameGenerator.repl_cpu [] 15 in
  print_endline (cpu1_color ^ "CPU1 score: " ^ string_of_int frst_cpu);
  print_endline
    ("\n" ^ cpu1_color
   ^ "CPU1's turn just finished. Scroll up to see thier cards and final score. \
      Enter any key to continue.");
  let _ = read_line () in
  print_endline ("\n" ^ cpu2_color ^ "Starting CPU2's turn");
  let second_cpu = GameGenerator.repl_cpu [] 17 in
  print_endline (cpu2_color ^ "CPU2 score " ^ string_of_int second_cpu);
  print_endline
    ("\n" ^ cpu2_color
   ^ "CPU2's turn just finished. Scroll up to see thier cards and final score. \
      Enter any key to continue.");
  let _ = read_line () in
  print_endline ("\n" ^ cpu3_color ^ "Starting CPU3's turn");
  let third_cpu = GameGenerator.third_input_cpu frst_cpu second_cpu [] in
  print_endline (cpu3_color ^ "CPU3 score: " ^ string_of_int third_cpu);
  print_endline
    ("\n" ^ cpu3_color
   ^ "CPU3's turn just finished. Scroll up to see thier cards and final score. \
      Enter any key to continue.");
  let _ = read_line () in
  print_endline ("\n" ^ cpu4_color ^ "Starting CPU4's turn");
  let fourth_cpu =
    GameGenerator.fourth_input_cpu frst_cpu second_cpu third_cpu []
  in
  print_endline (cpu4_color ^ "CPU4 score: " ^ string_of_int fourth_cpu);
  print_endline
    ("\n" ^ cpu4_color
   ^ "CPU4's turn just finished. Scroll up to see thier cards and final score. \
      Enter any key to continue.");
  let in4 = read_line () in
  print_endline in4;
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline
    (system_color
   ^ "Before you start playing, would you like to add more money to your bet? \
      (Enter a nonnegative interger representing how much more money you would \
      like to add)");
  let morebet =
    try read_int ()
    with Failure _ ->
      print_endline "Invalid input. Ending program.";
      exit 1
  in
  let morebet =
    match morebet with
    | x ->
        if x < 0 then (
          print_endline "Invalid input. Ending program.";
          exit 1)
        else x
  in
  let weightp =
    match morebet with
    | x -> x
  in
  let finalbet = weightp + paly_bet1 in

  print_endline (player_color ^ "Your turn:");
  print_endline (player_color ^ "Press H to hit, S to stay, or E to exit");
  let player_val = GameGenerator.repl [] in
  print_endline
    (player_color ^ "Your final score was: " ^ string_of_int player_val
   ^ "\nEnter any key to go to the dealer turn.");
  let _ = read_line () in
  print_endline ("\n" ^ system_color ^ "Starting dealer's turn");
  let dealer_val =
    GameGenerator.dealer_player frst_cpu second_cpu third_cpu fourth_cpu
      player_val []
  in
  print_endline (system_color ^ "Dealer's score: " ^ string_of_int dealer_val);
  print_endline
    (system_color
   ^ "The dealer's turn just finished. Scroll up to see thier cards and final \
      score. Enter any key to see the final results.");
  let _ = read_line () in
  print_endline (system_color ^ "Showing CPU1's results:");
  if frst_cpu > dealer_val then
    GameGenerator.winner false (cpu1_color ^ "CPU1")
      (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false (cpu1_color ^ "CPU1");
  print_endline
    (system_color
   ^ "CPU1's results are being shown. Enter any key to see CPU2's results.");
  let _ = read_line () in
  print_endline (system_color ^ "Showing CPU2's results:");
  if second_cpu > dealer_val then
    GameGenerator.winner false (cpu2_color ^ "CPU2")
      (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false (cpu2_color ^ "CPU2");
  print_endline
    "CPU2's results are being shown. Enter any key to see CPU3's results.";
  let _ = read_line () in
  print_endline (system_color ^ "Showing CPU3's results:");
  print_endline "";
  print_endline "";
  if third_cpu > dealer_val then
    GameGenerator.winner false (cpu3_color ^ "CPU3")
      (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false (cpu3_color ^ "CPU3");
  print_endline
    "CPU3's results are being shown. Enter any key to see CPU4's results.";
  let _ = read_line () in
  print_endline (system_color ^ "Showing CPU4's results:");
  print_endline "";
  print_endline "";
  if fourth_cpu > dealer_val then
    GameGenerator.winner false (cpu4_color ^ "CPU4")
      (string_of_int (cpu_bet * 2))
  else GameGenerator.loser false (cpu4_color ^ "CPU4");
  print_endline
    "CPU4's results are being shown. Enter any key to see YOUR results.";
  let _ = read_line () in
  print_endline (system_color ^ "Showing YOUR results:");
  print_endline "";
  print_endline "";
  if player_val > dealer_val then
    GameGenerator.winner true (player_color ^ "Player")
      (string_of_int (finalbet * 2))
  else GameGenerator.loser true (player_color ^ "Player");
  print_endline "Your results are being shown. Scroll up to see how you fared."
