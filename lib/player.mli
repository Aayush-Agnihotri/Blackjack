module type playerprofile = sig

  (** Value representing the current sum of the dealers cards *)
  val sum: int 

  (** Function that adds a specified int to the players sum  *)
  val add_to_sum : int -> int

  end

