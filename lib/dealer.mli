(** The signature of a dealer player *)

module Dealer : sig
  val sum : int ref
  (** Represents the sum of the dealer's hand *)

  val add_to_sum : int -> unit
  (** [add_to_sum n] adds [n] to the dealer's sum *)

  val num_of_players : int
  (** [num_of_players] returns the total number of players in the current game *)

  val set_players : int -> string -> unit
  (** [set_players n name] sets the current sum of player [name] to their
      previous value plus [n] *)

  val check_more_cards : bool
  (** [check_more_cards] returns whether the dealer should continue to hit or
      not *)
end
