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

  let print d =
    print_endline "------------------------";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_string "          ";
    print_string (string_of_int d.number);
    print_string "          ";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "|                      |";
    print_endline "------------------------"
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
