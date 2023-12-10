(** The signature for generating the endgame graphics *)

module Person_Gen : sig
  val print_large_stick_figure_smile : unit -> unit
  (** [print_large_stick_figure_smile ()] prints a stick figure with a smile. *)

  val print_large_stick_figure_nosmile : unit -> unit
  (** [print_large_stick_figure_nosmile ()] prints a stick figure with a neutral
      face. *)

  val print_money : string -> unit
  (** [print_money s] prints a large dollar sign with the string [s] in it. *)

  val print_winner : unit -> unit
  (** [print_winner ()] prints the message "Congrats! You are a Winner". *)

  val print_loser : unit -> unit
  (** [print_loser ()] prints the message "Sorry you lost". *)
end
