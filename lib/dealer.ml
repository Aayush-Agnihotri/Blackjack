type otherplayers = {
  firstcpu : int ref;
  secondcpu : int ref;
  thirdcpu : int ref;
  fourthcpu : int ref;
  player : int ref;
}

module type Dealerprofile = sig
  val sum : int ref
  val add_to_sum : int -> unit
  val otherplayerssums : otherplayers
  val num_of_players : int
  val set_players : int -> string -> unit
  val check_more_cards : bool
end

module Dealer : Dealerprofile = struct
  let sum = ref 0
  let add_to_sum (num : int) = sum := !sum + num
  let num_of_players = 5

  let otherplayerssums =
    {
      firstcpu = ref 0;
      secondcpu = ref 0;
      thirdcpu = ref 0;
      fourthcpu = ref 0;
      player = ref 0;
    }

  let set_players (updatenum : int) (playerval : string) =
    match playerval with
    | "first" ->
        otherplayerssums.firstcpu := !(otherplayerssums.firstcpu) + updatenum
    | "second" ->
        otherplayerssums.secondcpu := !(otherplayerssums.secondcpu) + updatenum
    | "third" ->
        otherplayerssums.thirdcpu := !(otherplayerssums.thirdcpu) + updatenum
    | "fourth" ->
        otherplayerssums.fourthcpu := !(otherplayerssums.fourthcpu) + updatenum
    | "player" ->
        otherplayerssums.player := !(otherplayerssums.player) + updatenum
    | _ -> ()

  let count_num_cards : int =
    let firstval = if !(otherplayerssums.firstcpu) > !sum then 0 else 1 in
    let secondval = if !(otherplayerssums.secondcpu) > !sum then 0 else 1 in
    let thirdval = if !(otherplayerssums.thirdcpu) > !sum then 0 else 1 in
    let fourthval = if !(otherplayerssums.fourthcpu) > !sum then 0 else 1 in
    let playerval = if !(otherplayerssums.player) > !sum then 0 else 1 in
    firstval + secondval + thirdval + fourthval + playerval

  let check_more_cards = if count_num_cards > 2 then true else false
end
