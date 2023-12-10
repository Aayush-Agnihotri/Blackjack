(* open Blackjack *)
module SpotCardGenerator = Card.SpotCard
module FaceCardGenerator = Card.FaceCard
module PersonGenerator = Person.Person_Gen

module Fullgamegen = struct
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

  let rec repl player_hand : int =
    print_string "> ";
    let input = read_line () in
    match input with
    | "H" ->
        print_endline "You chose to hit";
        let rand_num = 2 + Random.int 8 in
        let rand_suit = Random.int 4 in
        generate_card rand_num rand_suit;

        let player_hand_sum = List.fold_left ( + ) 0 player_hand + rand_num in
        if player_hand_sum > 21 then (
          print_endline "You busted!";
          0)
        else if player_hand_sum = 21 then (
          print_endline "You got 21!";
          21)
        else (
          print_endline
            ("Your hand's current value is " ^ string_of_int player_hand_sum);
          repl (rand_num :: player_hand))
    | "S" ->
        print_endline "You chose to stay";
        List.fold_left ( + ) 0 player_hand
    | _ ->
        print_endline "Thanks for playing!";
        List.fold_left ( + ) 0 player_hand

  let rec repl_cpu cpu_hand threshold =
    print_string "> ";
    let cpu_sum = List.fold_left ( + ) 0 cpu_hand in

    let input = cpu_sum < threshold in
    match input with
    | true ->
        print_endline "CPU chose to hit";
        let rand_num = 2 + Random.int 8 in
        let rand_suit = Random.int 4 in
        generate_card rand_num rand_suit;

        let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
        if player_hand_sum > 21 then (
          print_endline "CPU busted!";
          0)
        else if player_hand_sum = 21 then (
          print_endline "CPU won!";
          21)
        else (
          print_endline
            ("CPU hand's current value is " ^ string_of_int player_hand_sum);
          repl_cpu (rand_num :: cpu_hand) threshold)
    | false ->
        print_endline "CPU chose to stay";
        cpu_sum

  let rec decide_2 first second currenthand =
          let failed = ref 0 in
          if first = 0 then failed := !failed + 1 else failed := !failed;
          if second = 0 then failed := !failed + 1 else failed := !failed;
          if !failed > 1 then 15
          else
            let max_total = max first second in
            if max_total > 20 then 15 else if max_total < 15 then 15 else max_total
        (** This represents a player who uses the outputs of the first two players to determine wheterer they still want to get more cards*)
  let rec third_input_cpu first second cpu_hand =
          print_string "> ";
          let cpu_sum = List.fold_left ( + ) 0 cpu_hand in
          let decide_num = decide_2 first second cpu_sum in
          let input = cpu_sum < decide_num in
          match input with
          | true ->
              print_endline "CPU 3 chose to hit";
              let rand_num = 2 + Random.int 8 in
              let rand_suit = Random.int 4 in
              generate_card rand_num rand_suit;
        
              let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
              if player_hand_sum > 21 then (
                print_endline "CPU 3 busted!";
                0)
              else if player_hand_sum = 21 then (
                print_endline "CPU 3 won!";
                21)
              else (
                print_endline
                  ("CPU 3hand's current value is " ^ string_of_int player_hand_sum);
                third_input_cpu first second (rand_num :: cpu_hand))
          | false ->
              print_endline "CPU chose to stay";
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
              
              (** This represents a player who uses the outputs of the first three players to determine wheterer they still want to get more cards*)
    let rec fourth_input_cpu first second third cpu_hand =
                print_string "> ";
                let cpu_sum = List.fold_left ( + ) 0 cpu_hand in
                let decide_num = decide_3 first second third cpu_sum in
                let input = cpu_sum < decide_num in
                match input with
                | true ->
                    print_endline "CPU 4 chose to hit";
                    let rand_num = 2 + Random.int 8 in
                    let rand_suit = Random.int 4 in
                    generate_card rand_num rand_suit;
              
                    let player_hand_sum = List.fold_left ( + ) 0 cpu_hand + rand_num in
                    if player_hand_sum > 21 then (
                      print_endline "CPU 4 busted!";
                      0)
                    else if player_hand_sum = 21 then (
                      print_endline "CPU 4 won!";
                      21)
                    else (
                      print_endline
                        ("CPU 4 hand's current value is " ^ string_of_int player_hand_sum);
                      fourth_input_cpu first second third (rand_num :: cpu_hand))
                | false ->
                    print_endline "CPU 4 chose to stay";
                    cpu_sum
      let rec dealer_decide first second third fourth player dealer_val : bool =
                      let failed = ref 0 in
                      if first = 0 then failed := !failed + 1 else failed := !failed;
                      if second = 0 then failed := !failed + 1 else failed := !failed;
                      if third = 0 then failed := !failed + 1 else failed := !failed;
                      if fourth = 0 then failed := !failed + 1 else failed := !failed;
                      if player = 0 then failed := !failed + 1 else failed := !failed;
                    
                      if !failed > 4 then true
                      else
                        let count = ref 0 in
                        if dealer_val > first then count := !count + 1 else count := !count;
                        if dealer_val > second then failed := !count + 1 else count := !count;
                        if dealer_val > third then count := !count + 1 else count := !count;
                        if dealer_val > fourth then count := !count + 1 else count := !count;
                        if dealer_val > player then count := !count + 1 else count := !count;
                        if !count > 4 then false
                        else if !count > 3 && dealer_val > player then false
                        else if !count > 2 && dealer_val > 15 then false
                        else true
                    (** Represents a dealer player who uses the outputs of all the other players to select the best choice of wheter they should still keep taking cards.*)
        let rec dealer_player first second third fourth player dealer_hand =
                      print_string "> ";
                      let dealer_sum = List.fold_left ( + ) 0 dealer_hand in
                      (* let decide_num = decide_3 first second third cpu_sum in *)
                      let input : bool =
                        dealer_decide first second third fourth player dealer_sum
                      in
                      match input with
                      | true ->
                          print_endline "Dealer chose to hit";
                          let rand_num = 2 + Random.int 8 in
                          let rand_suit = Random.int 4 in
                          generate_card rand_num rand_suit;
                    
                          let player_hand_sum = List.fold_left ( + ) 0 dealer_hand + rand_num in
                          if player_hand_sum > 21 then (
                            print_endline "Dealer busted!";
                            0)
                          else if player_hand_sum = 21 then (
                            print_endline "Dealer won!";
                            21)
                          else (
                            print_endline
                              ("Dealer hand's current value is " ^ string_of_int player_hand_sum);
                            dealer_player first second third fourth player (rand_num :: dealer_hand))
                      | false ->
                          print_endline "Dealer chose to stay";
                          dealer_sum
                    
        let winner name num= print_endline(name ^ " is a winner");
            PersonGenerator.print_large_stick_figure_smile();
            PersonGenerator.print_money num
                          
        let loser name= print_endline(name^ "Lost"); 
        PersonGenerator.print_large_stick_figure_nosmile(); 
        PersonGenerator.print_money "0"
end
