(** The signature of a CPU player *)

module CPU : sig
  val sum : int ref
  (** The sum of the cards in the CPU's hand *)

  val add_to_sum : int -> unit
  (** [add_to_sum n] adds [n] to the sum of the cards in the CPU's hand *)

  val check_more : bool
  (** [check_more] is true if the CPU wants to draw another card, and false
      otherwise *)
end
