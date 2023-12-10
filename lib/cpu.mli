module CPU : sig
  
  val sum : int ref
  (**Integer val that represents the players current value of their cards*)

  val add_to_sum : int -> unit
  (**Function that allows a user to add a card value to the current sum*)

  val check_more : bool
  (**Checks wheter the player should draw another card *)
end
