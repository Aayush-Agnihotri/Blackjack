open OUnit2
open Blackjack
open Card

open SpotCard
(** [pp_string s] pretty-prints string [s]. *)

let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

module CardTest = Card.SpotCard
module FaceCardTest = Card.FaceCard

let card_tests =
  [
    ( "Card Value is 2" >:: fun _ ->
      let card = CardTest.create 2 Clubs in
      assert_equal (CardTest.value card) 2 );
    ( "Card Value is 3" >:: fun _ ->
        let card = CardTest.create 3 Clubs in
        assert_equal (CardTest.value card) 3 );
    ( "Card Value is 4" >:: fun _ ->
          let card = CardTest.create 4 Clubs in
          assert_equal (CardTest.value card) 4 );
    ( "Card Value is 5" >:: fun _ ->
      let card = CardTest.create 5 Clubs in
      assert_equal (CardTest.value card) 5 );
    ( "Card Value is 6" >:: fun _ ->
        let card = CardTest.create 6 Clubs in
        assert_equal (CardTest.value card) 6 );
    ( "Card Value is 7" >:: fun _ ->
          let card = CardTest.create 7 Clubs in
          assert_equal (CardTest.value card) 7 );
    ( "Card Value is 8" >:: fun _ ->
            let card = CardTest.create 8 Clubs in
            assert_equal (CardTest.value card) 8 );
    ( "Card Value is 9" >:: fun _ ->
              let card = CardTest.create 9 Clubs in
              assert_equal (CardTest.value card) 9 );
    ( "Card Value is 10" >:: fun _ ->
      let card = CardTest.create 10 Clubs in
      assert_equal (CardTest.value card) 10 );
    ( "Card Value is 5 and Suit is Clubs" >:: fun _ ->
      let card = CardTest.create 5 Clubs in
      assert_equal Clubs (CardTest.suit card) );
    ( "Card Value is 5 and Suit is Spades" >:: fun _ ->
      let card = CardTest.create 5 Spades in
      assert_equal Spades (CardTest.suit card) );
    ( "Card Value is 5 and Suit is Hearts" >:: fun _ ->
      let card = CardTest.create 5 Hearts in
      assert_equal Hearts (CardTest.suit card) );
    ( "Card Value is 5 and Suit is Diamonds" >:: fun _ ->
      let card = CardTest.create 5 Diamonds in
      assert_equal Diamonds (CardTest.suit card) );
    ( "Card Value is 5 and Suit is Jacks" >:: fun _ ->
      let card = CardTest.create 5 Spades in
      assert_equal Spades (CardTest.suit card) );
    ( "Card Value is 5 and Suit is Jacks" >:: fun _ ->
      let card = CardTest.create 5 Clubs in
      assert_bool "Suit should not be Spades" (suit card <> Spades) );
    ( "FaceCard Value is 13 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 13 Clubs in
      assert_equal (FaceCardTest.value card) 10 );
      ( "FaceCard Value is 14 and Suit is Clubs" >:: fun _ ->
        let card = FaceCardTest.create 14 Clubs in
        assert_equal (FaceCardTest.value card) 10 );
        ( "FaceCard Value is 15 and Suit is Clubs" >:: fun _ ->
          let card = FaceCardTest.create 15 Clubs in
          assert_equal (FaceCardTest.value card) 10 );
          ( "FaceCard Value is 16 and Suit is Clubs" >:: fun _ ->
            let card = FaceCardTest.create 16 Clubs in
            assert_equal (FaceCardTest.value card) 11 );
    ( "FaceCard Value is 13 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 13 Clubs in
      assert_equal (FaceCardTest.suit card) Clubs );
    ( "FaceCard Value is 13 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 13 Clubs in
      assert_equal (FaceCardTest.face card) Jack );
    ( "FaceCard Value is 14 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 14 Clubs in
      assert_equal (FaceCardTest.face card) Queen );
    ( "FaceCard Value is 15 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 15 Clubs in
      assert_equal (FaceCardTest.face card) King );
    ( "FaceCard Value is 16 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 16 Clubs in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 16 and Suit is Diamond" >:: fun _ ->
      let card = FaceCardTest.create 16 Diamonds in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 15 and Suit is Diamond" >:: fun _ ->
      let card = FaceCardTest.create 15 Diamonds in
      assert_equal (FaceCardTest.face card) King );
    ( "FaceCard Value is 14 and Suit is Diamond" >:: fun _ ->
      let card = FaceCardTest.create 14 Diamonds in
      assert_equal (FaceCardTest.face card) Queen );
    ( "FaceCard Value is 13 and Suit is Diamond" >:: fun _ ->
      let card = FaceCardTest.create 13 Diamonds in
      assert_equal (FaceCardTest.face card) Jack );
    ( "FaceCard Value is 16 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 16 Hearts in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 15 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 15 Hearts in
      assert_equal (FaceCardTest.face card) King );
    ( "FaceCard Value is 14 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 14 Hearts in
      assert_equal (FaceCardTest.face card) Queen );
    ( "FaceCard Value is 13 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 13 Hearts in
      assert_equal (FaceCardTest.face card) Jack );
    ( "FaceCard Value is 16 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 15 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 15 Spades in
      assert_equal (FaceCardTest.face card) King );
    ( "FaceCard Value is 14 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 14 Spades in
      assert_equal (FaceCardTest.face card) Queen );
    ( "FaceCard Value is 13 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 13 Spades in
      assert_equal (FaceCardTest.face card) Jack );
    ( "FaceCard Value is 16 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 16 Hearts in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 16 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 16 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.suit card) Spades );
  ]

module GameTest = Gameone.Fullgamegen

let cpu_tests =
  [
    ( "Testing addition for rep1" >:: fun _ ->
      assert_equal (GameTest.repl_cpu [ 3; 2; 1; 2; 10 ] 15) 18 );
    ( "testing whether functionality works for repl cpu" >:: fun _ ->
      assert_equal (0 < GameTest.repl_cpu [] 15) (GameTest.repl_cpu [] 15 < 21)
    );
    (*third input testing*)
    ( "third input CPU testing functionality 1" >:: fun _ ->
      assert_equal
        (0 < GameTest.third_input_cpu 18 17 [])
        (GameTest.third_input_cpu 18 17 [] >= 18) );
    ( "third input CPU testing functionality 2" >:: fun _ ->
      assert_equal
        (0 < GameTest.third_input_cpu 13 17 [])
        (GameTest.third_input_cpu 18 17 [] >= 17) );
    ( "third input CPU testing functionality 3" >:: fun _ ->
      assert_equal
        (0 < GameTest.third_input_cpu 13 6 [])
        (GameTest.third_input_cpu 18 17 [] >= 13) );
    ( "third input CPU testing functionality 4" >:: fun _ ->
      assert_equal
        (0 < GameTest.third_input_cpu 5 17 [])
        (GameTest.third_input_cpu 18 17 [] >= 17) );
    ( "third input CPU testing functionality 5" >:: fun _ ->
      assert_equal
        (0 > GameTest.third_input_cpu 21 17 [])
        (GameTest.third_input_cpu 18 17 [] > 21) );
    (*fourth input testing*)
    ( "fourth input CPU testing functionality 1" >:: fun _ ->
      let tester = GameTest.fourth_input_cpu 21 17 5 [] in
      assert_equal (0 <= tester) (21 > tester) );
    ( "fourth input CPU testing functionality 2" >:: fun _ ->
      let tester1 = GameTest.fourth_input_cpu 3 17 5 [] in
      assert_equal (0 <= tester1) (21 > tester1) );
    ( "fourth input CPU testing functionality 3" >:: fun _ ->
      let tester = GameTest.fourth_input_cpu 3 15 5 [] in
      assert_equal (0 <= tester) (0 < tester) );
    ( "fourth input CPU testing functionality 4" >:: fun _ ->
      let tester = GameTest.fourth_input_cpu 3 7 5 [] in
      assert_equal (0 <= tester) (21 > tester) );
    ( "fourth input CPU testing functionality 5" >:: fun _ ->
      let tester = GameTest.fourth_input_cpu 3 1 5 [] in
      assert_equal (0 <= tester) (21 > tester) );
  ]

let dealer_tests =
  [
    ( "dealer decide 1" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 0 0 0 0 0 0) true );
    ( "dealer decide 2" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 1 1 1 1 0 0) true );
    ( "dealer decide 3" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 1 1 1 1 1 0) true );
    ( "dealer decide 4" >:: fun _ ->
        assert_equal (GameTest.dealer_decide 1 1 1 0 0 0) true );
    ( "dealer decide 5" >:: fun _ ->
          assert_equal (GameTest.dealer_decide 1 1 0 0 0 0) true );
    ( "dealer decide 6" >:: fun _ ->
            assert_equal (GameTest.dealer_decide 1 0 0 0 0 0) true );
    ( "dealer decide 7" >:: fun _ ->
              assert_equal (GameTest.dealer_decide 10 10 10 10 10 0) true );
    ( "dealer decide 8" >:: fun _ ->
                assert_equal (GameTest.dealer_decide 10 10 10 10 0 0) true );
    ( "dealer decide 9" >:: fun _ ->
                  assert_equal (GameTest.dealer_decide 10 10 10 0 0 0) true );
    ( "dealer decide 10" >:: fun _ ->
                    assert_equal (GameTest.dealer_decide 13 10 0 0 0 0) true );
    ( "dealer decide 4" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 10 10 1 1 10 9) true );
    ( "dealer decide 5" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 10 10 1 1 10 11) false );
    ( "dealer decide 6" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 10 0 0 0 0 11) false );
    ( "dealer decide 7" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 0 10 0 0 0 11) false );
    ( "dealer decide 8" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 0 0 10 0 0 11) false );
    ( "dealer decide 9" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 0 0 0 10 0 11) false );
    ( "dealer decide 10" >:: fun _ ->
      assert_equal (GameTest.dealer_decide 0 0 0 0 10 11) false );



    ("decide_3 1" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 0 11) 13);
    ("decide_3 2" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 1 11) 15);
    ("decide_3 3" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 1 1 11) 15);
    ("decide_3 4" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 21 1 11) 15);
    ("decide_3 5" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 16 1 11) 16);
    ("decide_3 6" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 17 1 11) 17);
    ("decide_3 7" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 18 1 11) 18);
    ("decide_3 8" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 19 1 11) 19);
    ("decide_3 9" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 20 1 11) 20);
    ("decide_3 15" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 7 1 11) 15);
    ("decide_3 10" >:: fun _ -> 
      assert_equal (GameTest.decide_3 21 0 1 11) 15);
    ("decide_3 11" >:: fun _ -> 
      assert_equal (GameTest.decide_3 16 0 1 11) 16);
    ("decide_3 12" >:: fun _ -> 
      assert_equal (GameTest.decide_3 17 0 1 11) 17);
    ("decide_3 13" >:: fun _ -> 
      assert_equal (GameTest.decide_3 18 0 1 11) 18);
    ("decide_3 14" >:: fun _ -> 
      assert_equal (GameTest.decide_3 19 0 1 11) 19);
    ("decide_3 15" >:: fun _ -> 
      assert_equal (GameTest.decide_3 20 0 1 11) 20);
    ("decide_3 15" >:: fun _ -> 
      assert_equal (GameTest.decide_3 7 0 1 11) 15);
    ("decide_3 10" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 21 11) 15);
    ("decide_3 11" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 16 11) 16);
    ("decide_3 12" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 17 11) 17);
    ("decide_3 13" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 18 11) 18);
    ("decide_3 14" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 19 11) 19);
    ("decide_3 15" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 20 11) 20);
    ("decide_3 15" >:: fun _ -> 
      assert_equal (GameTest.decide_3 0 0 7 11) 15);

    ("decide_2 1" >:: fun _ -> 
      assert_equal (GameTest.decide_2 1 1 1) 15);
    ("decide_2 2" >:: fun _ -> 
      assert_equal (GameTest.decide_2 21 21 11) 15);
    ("decide_2 3" >:: fun _ -> 
      assert_equal (GameTest.decide_2 21 0 11) 15);
    ("decide_2 4" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 21 11) 15);
    ("decide_2 5" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 14 11) 15);
    ("decide_2 6" >:: fun _ -> 
      assert_equal (GameTest.decide_2 14 0 11) 15);
    ("decide_2 7" >:: fun _ -> 
      assert_equal (GameTest.decide_2 1 0 11) 15);
    ("decide_2 8" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 1 11) 15);
    ("decide_2 9" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 15 11) 15);
    ("decide_2 10" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 16 11) 16);
    ("decide_2 11" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 17 11) 17);
    ("decide_2 12" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 18 11) 18);
    ("decide_2 13" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 19 11) 19);
    ("decide_2 14" >:: fun _ -> 
      assert_equal (GameTest.decide_2 0 20 11) 20);
      ("decide_2 15" >:: fun _ -> 
        assert_equal (GameTest.decide_2 15 0 11) 15);
      ("decide_2 16" >:: fun _ -> 
        assert_equal (GameTest.decide_2 16 0 11) 16);
      ("decide_2 17" >:: fun _ -> 
        assert_equal (GameTest.decide_2 17 0 11) 17);
      ("decide_2 18" >:: fun _ -> 
        assert_equal (GameTest.decide_2 18 0 11) 18);
      ("decide_2 19" >:: fun _ -> 
        assert_equal (GameTest.decide_2 19 0 11) 19);
      ("decide_2 20" >:: fun _ -> 
        assert_equal (GameTest.decide_2 20 0 11) 20);

  ]

let player_tests = []

(* let game_tests = [ assert_equal 4 4; ] *)

let suite =
  "test suite" >::: List.flatten [ card_tests; cpu_tests; dealer_tests ]

let () = run_test_tt_main suite
