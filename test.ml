open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Types
open Lexing

let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx";;

let t name program expected = name>::test_run program name expected;;

let ta name program expected = name>::test_run_anf program name expected;;

let te name program expected_err = name>::test_err program name expected_err;;

let tanf name program expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_expr;;

let tstr name program expected = name>::fun _ ->
    assert_equal expected (compile_to_string program) ~printer:(fun s -> s);;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

let forty_one = "41";;

let forty_one_a = (ENumber(41, ()))

let suite =
"suite">:::
 [

  t "m1" "5 - 5" "0";
  t "m2" "5 + 5" "10";
  t "m3" "5 * 5" "25";
  t "m4" "5 - 0" "5";
  t "m5" "5 + 0" "5";
  t "m6" "5 * 0" "0";

  t "m7" "let x = 5 in x" "5";
  t "m8" "let x = 5, y = 6 in x + y" "11";
  t "m9" "let x = 5 + 6 in x" "11";
  t "m10" "let x = let y = 5 + 6 in y in x - 6" "5";
  t "m11" "let x = 5 in let y = 5 + x in y" "10";
  t "m12" "let x = 5, y = 6 in let z = x + y in z" "11";
  t "m13" "let x = 5, y = 6 in let z = let a = x + y in a in z" "11";

  t "m14" "let x = 5 in 5 * x" "25";
  t "m15" "let x = 5, y = 6 in x * y" "30";
  t "m16" "let x = 5, y = 6 in let z = let a = x * y in a in z" "30";

  t "if1" "if 5: 4 else: 2" "4";
  t "if2" "if 0: 4 else: 2" "2";
  t "if3" "let x = 5 in if x: 4 else: 5" "4";
  t "if4" "let x = 0 in if x: 4 else: 5" "5";
  t "if5" "let a = let x = 5, y = 16 in x + y in if a: 5 else: 0" "5";
  t "if6" "let a = let x = 5, y = 0 in x * y in if a: 5 else: 0" "0";
  t "if7" "let a = let x = 5, y = 1 in x * y in if a: 5 else: 0" "5";
  t "if8" "let a = let x = 6, y = 2 in x + y in
               if a: let x = 5, y = 6 in let z = let a = x + y in a in z
               else: let a = let x = 5, y = 1 in x * y in
                    if a: let x = 11 + 1 in x * 12
                    else: let  x = 6, y = 7 in x * y" "11";

  ]
;;


let () =
  run_test_tt_main suite
;;
