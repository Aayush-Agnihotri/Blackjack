(** The signature of a card *)
module type Card = sig
  type t
  (** Type representing the data of the card *)

  val create : t -> t
  (** [create x] creates a card with data [x] *)

  val print : t -> unit
  (** [print c] prints the card [c] in the terminal *)
end

type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type face =
  | Jack
  | Queen
  | King
  | Ace

module SpotCard : Card = struct
  type t = {
    number : int;
    suit : suit;
  }

  let create d = { number = d.number; suit = d.suit }

  let print_card_template n symbol =
    match n with
    | "2" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|     |";
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|____Z|"
    | "3" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|     |";
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|____e|"
    | "4" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|     |";
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____h|"
    | "5" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____S|"
    | "6" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____9|"
    | "7" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ symbol ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____L|"
    | "8" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ symbol ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ symbol ^ symbol ^ " |");
        print_endline "|____8|"
    | "9" ->
        print_endline " _____";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ symbol ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ symbol ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ symbol ^ symbol ^ " |");
        print_endline "|____6|"
    | _ -> print_endline ""

  let print card =
    match card with
    | { number; suit } ->
        print_card_template (string_of_int number)
          (match suit with
          | Clubs -> "&"
          | Diamonds -> "^"
          | Hearts -> "o"
          | Spades -> "v")
end

module FaceCard : Card = struct
  type t = {
    face : face;
    suit : suit;
    number : int;
  }

  let create d = { face = d.face; suit = d.suit; number = 1 }
  let print d = print_endline (string_of_int d.number)
end
