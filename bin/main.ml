open Blackjack

let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      input |> eval |> print_endline;
      repl eval

module Test_Card = Card.SpotCard

type a = Test_Card.suit

let all_cards = []
let player_cards = []
let _ = Random.self_init ()

let () =
  print_endline "\n\nWelcome to Blackjack!\n";
  print_endline "Press H to hit, S to stay, or E to exit";
  let input = read_line () in
  match input with
  | "H" ->
      print_endline "You chose to hit";
      let rand_num = Random.int 10 in
      Test_Card.print (Test_Card.create rand_num Test_Card.Clubs Test_Card.None)
  | "S" -> print_endline "You chose to stay"
  | _ -> print_endline "Thanks for playing"
