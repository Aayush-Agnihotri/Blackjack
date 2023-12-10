module Fullgamegen : sig
  val generate_card : int -> int -> unit
  val repl : int list -> int
  val repl_cpu : int list -> int -> int
  val decide_2 : int -> int -> int -> int
  val third_input_cpu : int -> int -> int list -> int
  val decide_3 : int -> int -> int -> int -> int
  val fourth_input_cpu : int -> int -> int -> int list -> int
  val dealer_decide : int -> int -> int -> int -> int -> int -> bool
  val dealer_player : int -> int -> int -> int -> int -> int list -> int
  val winner : bool -> string -> string -> unit
  val loser : bool -> string -> unit
  val example_game : int -> unit
end
