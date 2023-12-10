(** The signature of a game of Blackjack *)

module Game : sig
  val addToPlayer1 : unit -> unit
  (** [addToPlayer1 ()] adds a card to player 1's hand *)

  val addToCpu1 : unit -> unit
  (** [addToCpu1 ()] adds a card to cpu 1's hand *)

  val addToCpu2 : unit -> unit
  (** [addToCpu2 ()] adds a card to cpu 2's hand *)

  val addToCpu3 : unit -> unit
  (** [addToCpu3 ()] adds a card to cpu 3's hand *)

  val addToCpu4 : unit -> unit
  (** [addToCpu4 ()] adds a card to cpu 4's hand *)
end
