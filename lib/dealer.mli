module type Dealerprofile = sig

   (** Value representing the current sum of the dealers cards *) val sum: int

   (** Value representing a list of the other players sums at the tables *)

   val otherplayerssums: int list

   val add_to_sum : int -> int

   val check_more_cards : unit-> bool

   end
