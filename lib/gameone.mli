(** The signature of the game of Blackjack being played *)

module Fullgamegen : sig
  val generate_card : int -> int -> unit
  (** [generate_card x y] is the card that is generated for the player with the
      value [x] and the suit [y] *)

  val repl : int list -> int
  (** [repl lst] interprets what the player inputted and returns the new sum of
      the current hand, including the new card if the player hit *)

  val repl_cpu : int list -> int -> int
  (** [repl_cpu lst sum] interprets what the cpu inputted and returns the new
      sum of the current hand, including the new card if the cpu hit *)

  val decide_2 : int -> int -> int -> int
  (** [decide_2 x y z] is the sum of the player's hand after the player decides
      to hit or stay *)

  val third_input_cpu : int -> int -> int list -> int
  (** [third_input_cpu x y lst] is the sum of the cpu's hand after the cpu
      decides to hit or stay *)

  val decide_3 : int -> int -> int -> int -> int
  (** [decide_3 x y z a] is the sum of the player's hand after the player
      decides to hit or stay *)

  val fourth_input_cpu : int -> int -> int -> int list -> int
  (** [fourth_input_cpu x y z lst] is the sum of the cpu's hand after the cpu
      decides to hit or stay *)

  val dealer_decide : int -> int -> int -> int -> int -> int -> bool
  (** [dealer_decide x y z a b c] is true if the dealer decides to hit and false
      if the dealer decides to stay *)

  val winner : bool -> string -> string -> unit
  (** [winner b x y] prints the player [x] if [b] is true, along with their
      total score *)

  val loser : bool -> string -> unit
  (** [loser b x] prints the player [x] if [b] is true, along with their total
      score *)

  val example_game : int -> unit
  (** [example_game x] is the game of blackjack being played with [x] players *)

  val dealer_player : int -> int -> int -> int -> int -> int list -> int
  (** [dealer_player x y z a b lst] is the sum of the dealer's hand after the
      dealer decides to hit or stay *)
end
