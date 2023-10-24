open OUnit2
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

let card_tests = []
let cpu_tests = []
let dealer_tests = []
let player_tests = []

(* let game_tests = [ assert_equal 4 4; ] *)
let suite =
  "test suite"
  >::: List.flatten [ (* card_tests; cpu_tests; dealer_tests; player_tests *) ]

let () = run_test_tt_main suite
