module Fullgamegen : sig
  val generate_card : int -> int -> unit
  (** Creates card based on integer inputs and prints it to the terminal*)

  val repl : int list -> int
  (** Simulates that action of player playign the game using the terminal to
     decide wheter they want more cards.*)

  val repl_cpu : int list -> int -> int
  (** Simulates that action of regular cpu with a specificed threshold playign
     the game using the terminal to decide wheter they want more cards.*)

  val decide_2 : int -> int -> int -> int
  (** Decides that given two integers wheter a cpu should get another card.*)

  val third_input_cpu : int -> int -> int list -> int
  (** Simulates the third cpu playing the game using the other two cpu outcomes
     to decide wheter to continue playing.*)

  val decide_3 : int -> int -> int -> int -> int
  (** Decides that given three integers wheter a cpu should get another card.*)

  val fourth_input_cpu : int -> int -> int -> int list -> int
  (** Simulates the third cpu playing the game using the other three cpu outcomes
     to decide wheter to continue playing.*)

  val dealer_decide : int -> int -> int -> int -> int -> int -> bool
  (** Decides given all the other players wheter the dealer should draw for
     another card*)

  val dealer_player : int -> int -> int -> int -> int -> int list -> int
  (** Simulates the dealers hard*)

  val winner : bool -> string -> string -> unit
  (** Prints terminal output for a winner given name.*)

  val loser : bool -> string -> unit
  (** Prints terminal output for a loser given name.*)

  val example_game : int -> unit
  (** An exampel simulated game.*)
end
