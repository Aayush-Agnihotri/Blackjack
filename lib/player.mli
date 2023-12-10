(** The signature of a plyer *)

module Player : sig
  val sum : int ref
  (** [sum] is the total value of the cards a player holds *)

  val add_to_sum : int -> unit
  (** [add_to_sum x] adds [x] to the player's [sum] *)

  val get_boolean_input : unit -> bool
  (** [get_boolean_input ()] is the boolean input from the player *)
end
