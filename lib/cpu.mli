module CPU : sig
  (*Integer val that represents the players current value of their cards*)
  val sum : int ref
  (*Function that allows a user to add a card value to the current sum*)

  val add_to_sum : int -> unit
  (*Checks wheter the player should draw another card *)

  val check_more : bool
end
