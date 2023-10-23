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
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|     |";
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|____Z|"
    | "3" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|     |";
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|____e|"
    | "4" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|     |";
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____h|"
    | "5" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____S|"
    | "6" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____9|"
    | "7" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____L|"
    | "8" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline "|____8|"
    | "9" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline "|____6|"
    | "10" ->
        print_endline " _____ ";
        print_endline ("|" ^ n ^ " " ^ symbol ^ " |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline "|___0I|"
    | _ ->
        print_endline
          ("Unable to represent card of number " ^ n ^ " and suit " ^ symbol)

  let print card =
    match card with
    | { number; suit } ->
        print_card_template (string_of_int number)
          (match suit with
          | Clubs -> "&"
          | Diamonds -> "o"
          | Hearts -> "v"
          | Spades -> "^")
end

module FaceCard : Card = struct
  type t = {
    face : face;
    suit : suit;
  }

  let create d = { face = d.face; suit = d.suit }

  let print_card_template (f : face) (s : suit) =
    match f with
    | Jack -> print_endline ""
    | Queen -> print_endline ""
    | King -> print_endline ""
    | Ace ->
        (match s with
        | Clubs ->
            print_endline " _____ ";
            print_endline "|A _  |";
            print_endline "| ( ) |";
            print_endline "|(_'_)|";
            print_endline "|  |  |"
        | Diamonds ->
            print_endline " _____ ";
            print_endline "|A ^  |";
            print_endline "| / \\ |";
            print_endline "| \\ / |";
            print_endline "|  .  |"
        | Hearts ->
            print_endline " _____ ";
            print_endline "|A_ _ |";
            print_endline "|( v )|";
            print_endline "| \\ / |";
            print_endline "|  .  |"
        | Spades ->
            print_endline " _____ ";
            print_endline "|A .  |";
            print_endline "| /.\\ |";
            print_endline "|(_._)|";
            print_endline "|  |  |");
        print_endline "|____V|"

  let print card =
    match card with
    | { face; suit } -> print_card_template face suit
end
