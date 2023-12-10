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
    ( "Card Value is 5" >:: fun _ ->
      let card = CardTest.create 5 Clubs in
      assert_equal (CardTest.value card) 5 );
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
    ( "FaceCard Value is 13 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 16 Hearts in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 13 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.face card) Ace );
    ( "FaceCard Value is 13 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.suit card) Spades );
  ]

module GameTest = Gameone.Fullgamegen

let cpu_tests =
  [
    ( "Testing addition for rep1" >:: fun _ ->
      assert_equal (GameTest.repl_cpu [ 3; 2; 1; 2; 10 ]) 18 );
    ( "testing whether functionality works for repl cpu" >:: fun _ ->
      assert_equal (0 < GameTest.repl_cpu []) (GameTest.repl_cpu [] < 21) );
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
      assert_equal
        (0 < tester)
        (tester > 15) );
    ( "fourth input CPU testing functionality 2" >:: fun _ ->
      let tester1 = GameTest.fourth_input_cpu 3 17 5 [] in
      assert_equal
        (0 < tester1)
        (tester1 > 15) );
    ( "fourth input CPU testing functionality 3" >:: fun _ ->
      assert_equal
        (0 < GameTest.fourth_input_cpu 3 15 5 [])
        (GameTest.fourth_input_cpu 3 15 5 [] > 15) );
    ( "fourth input CPU testing functionality 4" >:: fun _ ->
      assert_equal
        (0 < GameTest.fourth_input_cpu 3 7 5 [])
        (GameTest.fourth_input_cpu 3 7 5 [] > 15) );
    ( "fourth input CPU testing functionality 5" >:: fun _ ->
      assert_equal
        (0 < GameTest.fourth_input_cpu 3 1 5 [])
        (GameTest.fourth_input_cpu 3 1 5 [] > 15) );

       
  ]

let dealer_tests = [ 
  ( "dealer decide 1" >:: fun _ ->
  assert_equal
    (GameTest.dealer_decide 0 0 0 0 0 0)
    (true) );
    ( "dealer decide 2" >:: fun _ ->
      assert_equal
        (GameTest.dealer_decide 1 1 1 1 0 0)
        (true) );
        ( "dealer decide 3" >:: fun _ ->
          assert_equal
            (GameTest.dealer_decide 1 1 1 1 1 0)
            (false) );
            ( "dealer decide 4" >:: fun _ ->
              assert_equal
                (GameTest.dealer_decide 10 10 1 1 10 9)
                (true) );
                ( "dealer decide 5" >:: fun _ ->
                  assert_equal
                    (GameTest.dealer_decide 10 10 1 1 10 11)
                    (false) );
                    ( "dealer decide 6" >:: fun _ ->
                      assert_equal
                        (GameTest.dealer_decide 10 0 0 0 0 11)
                        (true) );
                        ( "dealer decide 7" >:: fun _ ->
                          assert_equal
                            (GameTest.dealer_decide 0 10 0 0 0 11)
                            (true) );
                            ( "dealer decide 8" >:: fun _ ->
                              assert_equal
                                (GameTest.dealer_decide 0 0 10 0 0 11)
                                (true) );
                                ( "dealer decide 5" >:: fun _ ->
                                  assert_equal
                                    (GameTest.dealer_decide 0 0 0 10 0 11)
                                    (true) );
                                    ( "dealer decide 5" >:: fun _ ->
                                      assert_equal
                                        (GameTest.dealer_decide 0 0 0 0 10 11)
                                        (true) );
                    
    ]
let player_tests = []

(* let game_tests = [ assert_equal 4 4; ] *)

let suite =
  "test suite"
  >::: List.flatten
         [
           card_tests;
           cpu_tests (* card_tests; cpu_tests; dealer_tests; player_tests *);
         ]

let () = run_test_tt_main suite
