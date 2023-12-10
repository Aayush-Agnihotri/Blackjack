module Player : sig
  val sum : int ref

  (*Current sum representation.*)
  val add_to_sum : int -> unit
  (*Adds value to players current sum.*)

  val get_boolean_input : unit -> bool
  (*Decides wheter to draw another card.*)
end
