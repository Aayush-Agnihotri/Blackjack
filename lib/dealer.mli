module Dealer : sig
  val sum : int ref

  (* Represents the current sum of the dealer*)
  val add_to_sum : int -> unit

  (* Adds card to dealer's hand*)
  val num_of_players : int

  (* Represents number of players playing the game*)
  val set_players : int -> string -> unit

  (*Iniitalizes the players for the game*)
  val check_more_cards : bool
  (* Checks if the dealer should take another card*)
end
