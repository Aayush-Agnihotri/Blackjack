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

let rec repl_cpu cpu_hand =
  print_string "> ";
  let cpu_sum = List.fold_left ( + ) 0 cpu_hand in

  let input = cpu_sum < 17 in
  match input with
  | true ->
      print_endline "You chose to hit";
      let rand_num = 2 + Random.int 8 in
      let rand_suit = Random.int 4 in
      generate_card rand_num rand_suit;

      let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
      if player_hand_sum > 21 then (
        print_endline "You busted!";
        0)
      else if player_hand_sum = 21 then (
        print_endline "You win!";
        21)
      else (
        print_endline
          ("Your hand's current value is " ^ string_of_int player_hand_sum);
        repl_cpu (rand_num :: cpu_hand))
  | false ->
      print_endline "You chose to stay";
      cpu_sum

let rec repl_cpu cpu_hand =
  print_string "> ";
  let cpu_sum = List.fold_left ( + ) 0 cpu_hand in

  let input = cpu_sum < 15 in
  match input with
  | true ->
      print_endline "You chose to hit";
      let rand_num = 2 + Random.int 8 in
      let rand_suit = Random.int 4 in
      generate_card rand_num rand_suit;

      let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
      if player_hand_sum > 21 then (
        print_endline "You busted!";
        0)
      else if player_hand_sum = 21 then (
        print_endline "You win!";
        21)
      else (
        print_endline
          ("Your hand's current value is " ^ string_of_int player_hand_sum);
        repl_cpu (rand_num :: cpu_hand))
  | false ->
      print_endline "You chose to stay";
      cpu_sum

let rec aggresive_repl_cpu cpu_hand =
  print_string "> ";
  let cpu_sum = List.fold_left ( + ) 0 cpu_hand in

  let input = cpu_sum < 17 in
  match input with
  | true ->
      print_endline "You chose to hit";
      let rand_num = 2 + Random.int 8 in
      let rand_suit = Random.int 4 in
      generate_card rand_num rand_suit;

      let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
      if player_hand_sum > 21 then (
        print_endline "You busted!";
        0)
      else if player_hand_sum = 21 then (
        print_endline "You win!";
        21)
      else (
        print_endline
          ("Your hand's current value is " ^ string_of_int player_hand_sum);
        repl_cpu (rand_num :: cpu_hand))
  | false ->
      print_endline "You chose to stay";
      cpu_sum

let rec decide_2 first second currenthand =
  let failed = ref 0 in
  if first = 0 then failed := !failed + 1 else failed := !failed;
  if second = 0 then failed := !failed + 1 else failed := !failed;
  if !failed > 1 then 15
  else
    let max_total = max first second in
    if max_total > 20 then 15 else if max_total < 15 then 15 else max_total

let rec third_input_cpu first second cpu_hand =
  print_string "> ";
  let cpu_sum = List.fold_left ( + ) 0 cpu_hand in
  let decide_num = decide_2 first second cpu_sum in
  let input = cpu_sum < decide_num in
  match input with
  | true ->
      print_endline "You chose to hit";
      let rand_num = 2 + Random.int 8 in
      let rand_suit = Random.int 4 in
      generate_card rand_num rand_suit;

      let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
      if player_hand_sum > 21 then (
        print_endline "You busted!";
        0)
      else if player_hand_sum = 21 then (
        print_endline "You win!";
        21)
      else (
        print_endline
          ("Your hand's current value is " ^ string_of_int player_hand_sum);
        repl_cpu (rand_num :: cpu_hand))
  | false ->
      print_endline "You chose to stay";
      cpu_sum

let rec decide_3 first second third currenthand =
  let failed = ref 0 in
  if first = 0 then failed := !failed + 1 else failed := !failed;
  if second = 0 then failed := !failed + 1 else failed := !failed;
  if third = 0 then failed := !failed + 1 else failed := !failed;

  if !failed > 2 then 13
  else
    let max_total1 = max first second in
    let max_total = max max_total1 third in
    if max_total > 20 then 15 else if max_total < 15 then 15 else max_total

let rec fourth_input_cpu first second third cpu_hand =
  print_string "> ";
  let cpu_sum = List.fold_left ( + ) 0 cpu_hand in
  let decide_num = decide_3 first second third cpu_sum in
  let input = cpu_sum < decide_num in
  match input with
  | true ->
      print_endline "You chose to hit";
      let rand_num = 2 + Random.int 8 in
      let rand_suit = Random.int 4 in
      generate_card rand_num rand_suit;

      let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
      if player_hand_sum > 21 then (
        print_endline "You busted!";
        0)
      else if player_hand_sum = 21 then (
        print_endline "You win!";
        21)
      else (
        print_endline
          ("Your hand's current value is " ^ string_of_int player_hand_sum);
        repl_cpu (rand_num :: cpu_hand))
  | false ->
      print_endline "You chose to stay";
      cpu_sum

let () =
  print_endline "\n\nWelcome to Blackjack!\n";
  let frst_cpu = repl_cpu [] in
  print_endline ("First cpu score " ^ string_of_int frst_cpu);
  let second_cpu = aggresive_repl_cpu [] in
  print_endline ("Second cpu score " ^ string_of_int second_cpu);
  let third_cpu = third_input_cpu frst_cpu second_cpu [] in
  print_endline ("Third cpu score " ^ string_of_int third_cpu);
  let fourth_cpu = fourth_input_cpu frst_cpu second_cpu third_cpu [] in
  print_endline ("Fourth cpu score " ^ string_of_int fourth_cpu);

  print_endline "Press H to hit, S to stay, or E to exit";
  repl []
