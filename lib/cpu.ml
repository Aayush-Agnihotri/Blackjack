module type Playerprofile = sig
  (** Itteger val that represents the players current  value of their cards*)
  val sum : int ref
  (** Function that allows a user to add a card value to the current sum*)
  val add_to_sum: int -> unit 
  (**Checks wheter the player should draw another card *)
  val  check_more: bool
end
module Player : Playerprofile = struct
  let sum = ref 0
  let add_to_sum (num:int)= sum:= !sum +num 
  let check_more= if !sum < 17 then true else false; 
end
