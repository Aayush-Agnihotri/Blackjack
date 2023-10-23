module type Playerprofile = sig
(*Integer val that represents the players current value of their cards*)
  val sum : int ref
(*Function that allows a user to add a card value to the current sum*)

  val add_to_sum : int -> unit
(*Checks wheter the player should draw another card *)
  val get_boolean_input : unit -> bool

end

module Player: Playerprofile  = struct
  let sum = ref 0
  let add_to_sum (num : int) = sum := !sum + num
  let get_boolean_input () =
    print_string "Enter 'y' or 'n': ";
    flush stdout;

    let user_input = read_line () in

    match user_input with
    | "y" -> true
    | "n" -> false
    | _ -> false
  ;;

end (* This will be a boolean value: true for 'y', false for 'n', and false for any other input *)


