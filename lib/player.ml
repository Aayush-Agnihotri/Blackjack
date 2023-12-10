module type Playerprofile = sig
  val sum : int ref
  val add_to_sum : int -> unit
  val get_boolean_input : unit -> bool
end

module Player : Playerprofile = struct
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
end
