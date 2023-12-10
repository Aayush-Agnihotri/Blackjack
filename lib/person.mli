module Person_Gen : sig
  val print_large_stick_figure_smile : unit -> unit
  (** print_large_stick_figure_smile creates a stick figure representation of a
      human head that is smiling *)

  val print_large_stick_figure_nosmile : unit -> unit
  (** print_large_stick_figure_smile creates a stick figure representation of a
      human head that is not smiling *)
  val print_money: string -> unit
  (* Given an integer prints money representing that integer*)
  val  print_winner: unit -> unit
  (* Prints Congratulations output to terminal*)
  val print_loser: unit -> unit
    (* Prints sorry message to terminal output.*)

end
