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
    ("Card Value is 5 and Suit is Jacks" >:: fun _ ->
      let card = CardTest.create 5 Spades in
      assert_equal (Spades) (CardTest.suit card) );
    
      ]
    
let cpu_tests = []
let dealer_tests = []
let player_tests = []

(* let game_tests = [ assert_equal 4 4; ] *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list pp_string)
      [ "foo"; "bar" ] [ "bar"; "foo" ] );
    (* Uncomment this test to see what happens when a test case fails.
      ( "counts must be the same" >:: fun _ -> assert_equal
       ~cmp:cmp_bag_like_lists ~printer:(pp_list pp_string) ["foo"; "foo"]
      ["foo"]); *)
  ]
let suite =
  "test suite"
  >::: List.flatten [ cmp_demo; card_tests(* card_tests; cpu_tests; dealer_tests; player_tests *) ]

let () = run_test_tt_main suite
