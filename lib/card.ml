module type Card = sig
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
    | None

  type t

  val create : int -> suit -> t
  val value : t -> int
  val suit : t -> suit
  val print : t -> unit
end

module SpotCard: Card = struct
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
    | None

  type t = {
    number : int;
    suit : suit;
  }

  let create (n : int) (s : suit) = { number = n; suit = s }

  let value (card : t) =
    match card with
    | { number; _ } -> number

  let suit (card : t) =
    match card with
    | { suit; _ } -> suit

  let print_suit (card: t) =
    match card with
    | { suit; _ } -> match suit with 
      | Clubs -> "Clubs"
      | Diamonds -> "Diamonds"
      | Hearts -> "Hearts"
      | Spades -> "Spades"

    let print_create (n : int) (s : suit) =
    {
      number = n;
      suit = s;
    }
  let print_card_template (n : string) (symbol : string) =
    print_endline " _____ ";
    match n with
    | "2" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|     |";
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|____Z|"
    | "3" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|     |";
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline "|____e|"
    | "4" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|     |";
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____h|"
    | "5" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|  " ^ symbol ^ "  |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____S|"
    | "6" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____9|"
    | "7" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline "|____L|"
    | "8" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("| " ^ symbol ^ " " ^ symbol ^ " |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline "|____8|"
    | "9" ->
        print_endline ("|" ^ n ^ "    |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline "|____6|"
    | "10" ->
        print_endline ("|" ^ n ^ " " ^ symbol ^ " |");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline ("|" ^ symbol ^ " " ^ symbol ^ " " ^ symbol ^ "|");
        print_endline "|___0I|"
    | _ ->
        print_endline
          ("Unable to represent card of number " ^ n ^ " and suit " ^ symbol)

  let print (card : t) =
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
    | None

  type t = {
    face : face;
    suit : suit;
  }

  let create (n : int) (s : suit) =
    {
      face =
        (match n with
        | 13 -> Jack
        | 14 -> Queen
        | 15 -> King
        | 16 -> Ace
        | _ -> None);
      suit = s;
    }

  let value (card : t) =
    match card with
    | { face; _ } -> (
        match face with
        | Jack -> 10
        | Queen -> 10
        | King -> 10
        | Ace -> 11
        | None -> 0)

  let suit (card : t) =
    match card with
    | { suit; _ } -> suit

    let print_suit (card: t) =
      match card with
      | { suit; _ } -> match suit with 
        | Clubs -> "Clubs"
        | Diamonds -> "Diamonds"
        | Hearts -> "Hearts"
        | Spades -> "Spades"
  let print_card_template (f : face) (s : suit) =
    print_endline " _____ ";
    match f with
    | Jack ->
        print_endline "|J  ww|";
        (match s with
        | Clubs ->
            print_endline "| o {)|";
            print_endline "|o o% |";
            print_endline "| | % |"
        | Diamonds ->
            print_endline "| /\\{)|";
            print_endline "| \\/% |";
            print_endline "|   % |"
        | Hearts ->
            print_endline "|   {)|";
            print_endline "|(v)% |";
            print_endline "| v % |"
        | Spades ->
            print_endline "| ^ {)|";
            print_endline "|(.)% |";
            print_endline "| | % |");
        print_endline "|__%%[|"
    | Ace ->
        (match s with
        | Clubs ->
            print_endline "|A _  |";
            print_endline "| ( ) |";
            print_endline "|(_'_)|";
            print_endline "|  |  |"
        | Diamonds ->
            print_endline "|A ^  |";
            print_endline "| / \\ |";
            print_endline "| \\ / |";
            print_endline "|  .  |"
        | Hearts ->
            print_endline "|A_ _ |";
            print_endline "|( v )|";
            print_endline "| \\ / |";
            print_endline "|  .  |"
        | Spades ->
            print_endline "|A .  |";
            print_endline "| /.\\ |";
            print_endline "|(_._)|";
            print_endline "|  |  |");
        print_endline "|____V|"
    | None -> print_endline ""
    | _ ->
        if f = Queen then print_endline "|Q  ww|" else print_endline "|K  WW|";
        (match s with
        | Clubs ->
            print_endline "| o {(|";
            print_endline "|o o%%|";
            print_endline "| |%%%|"
        | Diamonds ->
            print_endline "| /\\{(|";
            print_endline "| \\/%%|";
            print_endline "|  %%%|"
        | Hearts ->
            print_endline "|   {(|";
            print_endline "|(v)%%|";
            print_endline "| v%%%|"
        | Spades ->
            print_endline "| ^ {(|";
            print_endline "|(.)%%|";
            print_endline "| |%%%|");
        if f = Queen then print_endline "|_%%%O|" else print_endline "|_%%%>|"

  let print (card : t) =
    match card with
    | { face; suit } -> print_card_template face suit
end
