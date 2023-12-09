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
  
let card_tests = [
  ( "Card Value is 5" >:: fun _ ->
    let card = CardTest.create 5 Clubs in
    assert_equal (CardTest.value card) (5) );
  ( "Card Value is 10" >:: fun _ -> 
    let card = CardTest.create 10 Clubs in
    assert_equal (CardTest.value card) (10) );
  ("Card Value is 5 and Suit is Clubs" >:: fun _ ->
  let card = CardTest.create 5 Clubs in
  assert_equal (Clubs) (CardTest.suit card) );
  ("Card Value is 5 and Suit is Spades" >:: fun _ ->
  let card = CardTest.create 5 Spades in
  assert_equal (Spades) (CardTest.suit card) );
  ("Card Value is 5 and Suit is Hearts" >:: fun _ ->
  let card = CardTest.create 5 Hearts in
  assert_equal (Hearts) (CardTest.suit card) );
  ("Card Value is 5 and Suit is Diamonds" >:: fun _ ->
  let card = CardTest.create 5 Diamonds in
  assert_equal (Diamonds) (CardTest.suit card) );
  ("Card Value is 5 and Suit is Jacks" >:: fun _ ->
    let card = CardTest.create 5 Spades in
    assert_equal (Spades) (CardTest.suit card) );
  ("Card Value is 5 and Suit is Jacks" >:: fun _ ->
    let card = CardTest.create 5 Clubs in
    assert_bool "Suit should not be Spades" (suit card <> Spades););
    
  ( "FaceCard Value is 13 and Suit is Clubs" >:: fun _ ->
    let card = FaceCardTest.create 13 Clubs in
    assert_equal (FaceCardTest.value card) (10) );
    ( "FaceCard Value is 13 and Suit is Clubs" >:: fun _ ->
      let card = FaceCardTest.create 13 Clubs in
      assert_equal (FaceCardTest.suit card) (Clubs) );
    ( "FaceCard Value is 13 and Suit is Clubs" >:: fun _ ->
        let card = FaceCardTest.create 13 Clubs in
        assert_equal (FaceCardTest.face card) (Jack) );
    ( "FaceCard Value is 14 and Suit is Clubs" >:: fun _ ->
          let card = FaceCardTest.create 14 Clubs in
          assert_equal (FaceCardTest.face card) (Queen) );
    ( "FaceCard Value is 15 and Suit is Clubs" >:: fun _ ->
            let card = FaceCardTest.create 15 Clubs in
            assert_equal (FaceCardTest.face card) (King) );
    ( "FaceCard Value is 16 and Suit is Clubs" >:: fun _ ->
    let card = FaceCardTest.create 16 Clubs in
    assert_equal (FaceCardTest.face card) (Ace) );

    ( "FaceCard Value is 16 and Suit is Diamond" >:: fun _ ->
      let card = FaceCardTest.create 16 Diamonds in
      assert_equal (FaceCardTest.face card) (Ace) );
      ( "FaceCard Value is 15 and Suit is Diamond" >:: fun _ ->
        let card = FaceCardTest.create 15 Diamonds in
        assert_equal (FaceCardTest.face card) (King) );
        ( "FaceCard Value is 14 and Suit is Diamond" >:: fun _ ->
          let card = FaceCardTest.create 14 Diamonds in
          assert_equal (FaceCardTest.face card) (Queen) );
          ( "FaceCard Value is 13 and Suit is Diamond" >:: fun _ ->
            let card = FaceCardTest.create 13 Diamonds in
            assert_equal (FaceCardTest.face card) (Jack) );

            ( "FaceCard Value is 16 and Suit is Hearts" >:: fun _ ->
              let card = FaceCardTest.create 16 Hearts in
              assert_equal (FaceCardTest.face card) (Ace) );
              ( "FaceCard Value is 15 and Suit is Hearts" >:: fun _ ->
                let card = FaceCardTest.create 15 Hearts in
                assert_equal (FaceCardTest.face card) (King) );
                ( "FaceCard Value is 14 and Suit is Hearts" >:: fun _ ->
                  let card = FaceCardTest.create 14 Hearts in
                  assert_equal (FaceCardTest.face card) (Queen) );
                  ( "FaceCard Value is 13 and Suit is Hearts" >:: fun _ ->
                    let card = FaceCardTest.create 13 Hearts in
                    assert_equal (FaceCardTest.face card) (Jack) );

                    ( "FaceCard Value is 16 and Suit is Spades" >:: fun _ ->
                      let card = FaceCardTest.create 16 Spades in
                      assert_equal (FaceCardTest.face card) (Ace) );
                      ( "FaceCard Value is 15 and Suit is Spades" >:: fun _ ->
                        let card = FaceCardTest.create 15 Spades in
                        assert_equal (FaceCardTest.face card) (King) );
                        ( "FaceCard Value is 14 and Suit is Spades" >:: fun _ ->
                          let card = FaceCardTest.create 14 Spades in
                          assert_equal (FaceCardTest.face card) (Queen) );
                          ( "FaceCard Value is 13 and Suit is Spades" >:: fun _ ->
                            let card = FaceCardTest.create 13 Spades in
                            assert_equal (FaceCardTest.face card) (Jack) );



    ( "FaceCard Value is 13 and Suit is Hearts" >:: fun _ ->
      let card = FaceCardTest.create 16 Hearts in
      assert_equal (FaceCardTest.face card) (Ace) );
    ( "FaceCard Value is 13 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.face card) (Ace) );
    ( "FaceCard Value is 13 and Suit is Spades" >:: fun _ ->
      let card = FaceCardTest.create 16 Spades in
      assert_equal (FaceCardTest.suit card) (Spades) );

    
      ]
    
let cpu_tests = []
let dealer_tests = []
let player_tests = []

(* let game_tests = [ assert_equal 4 4; ] *)

let suite =
  "test suite"
  >::: List.flatten [card_tests(* card_tests; cpu_tests; dealer_tests; player_tests *) ]

let () = run_test_tt_main suite
