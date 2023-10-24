open Blackjack
(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

module Test_Card = Card.SpotCard

type a = Test_Card.suit

let () =
  print_endline "Welcome to Blackjack!";
  print_endline "How many players do you want to play against?";
  print_string "> ";
  let input = read_line () in
  Test_Card.print
    (Test_Card.create (int_of_string input) Test_Card.Clubs Test_Card.None)
