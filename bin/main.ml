open Card

(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

let () =
  print_endline "Welcome to Blackjack!";
  print_endline "How many players do you want to play against?";
  print_string "> ";
  let input = read_line () in
  print_endline "Initializing game...";
  print_endline input
