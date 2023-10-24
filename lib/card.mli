(** The signature of a card *)
module type Card = sig
  (** Variant of the different suits *)
  type suit =
    | Clubs
    | Diamonds
    | Hearts
    | Spades

  (** Variant of the different faces *)
  type face =
    | Jack
    | Queen
    | King
    | Ace
    | None

  type t
  (** Type representing the data of the card *)

  val create : int -> suit -> face -> t
  (** [create v s f] creates a card with value [v], suit [s] and face [f] *)

  val value : t -> int
  (** [value c] returns the value of the card [c] *)

  val suit : t -> suit
  (** [suit c] returns the suit of the card [c] *)

  val print : t -> unit
  (** [print c] prints the card [c] in the terminal *)
end

module SpotCard : Card
(** Card with a suit and a value *)

module FaceCard : Card
(** Card with a suit and a face *)
