module Player : sig
  val sum : int ref
  val add_to_sum : int -> unit
  val get_boolean_input : unit -> bool
end
