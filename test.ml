open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Types
open Lexing

let is_osx = Conf.make_bool "osx" true "Set this flag to run on osx";;

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

  tanf "forty_one_anf"
       (ENumber(41, ()))
       forty_one_a;

  (*tanf "forty_one_anf"*)
       (*(EPrim2(Times, ENumber(42,()), ENumber(24,()), ()))*)
       (*forty_one_a;*)


  (*tanf "prim1_anf"
       (EPrim1(Sub1, EPrim1(Add1, ENumber(77,()), ()), ()))
       (ELet(["unary_1", EPrim1(Sub1, ENumber(55, ()), ()), ()],
             EId("unary_1", ()),
             ()));*)

  (*tstr "prim1_anf"*)
       (*(EPrim1(Sub1, EPrim1(Add1, ENumber(77,()), ()), ()))*)
       (*("test");*)

  (*tstr "if1"*)
        (*(EIf(ENumber(55, pos),*)
             (*EPrim2(Times, ENumber(12, pos), ENumber(24, pos), pos),*)
             (*EPrim1(Sub1, EPrim1(Add1, ENumber(77,pos), pos), pos), pos))*)
        (*("test");*)

  (*ta "forty_one_run_anf" (tag forty_one_a) "41";*)

  (*t "forty_one" forty_one "41";*)
  (*t "test" test_prog "3";*)

    (*(* Some useful if tests to start you off *)*)

  t "if1" "if 5: 4 else: 2" "4";
  t "if2" "if 0: 4 else: 2" "2";

  t "if3" "let x = 5 in if x: 4 else: 5" "4";
  t "if4" "let x = 0 in if x: 4 else: 5" "5";
  t "if5" "let a = let x = 5, y = 16 in x + y in if a: 5 else: 0" "5";

  t "m1" "5 - 5" "0";
  t "m2" "5 + 5" "10";
  t "m3" "let x = 5 in x" "5";

  t "m4" "let x = 5, y = 6 in x + y" "11";
  t "m5" "let x = 5 + 6 in x" "11";
  t "m6" "let x = let y = 5 + 6 in y in x - 6" "5";
  t "m7" "let x = 5 in let y = 5 + x in y" "10";
  t "m8" "let x = 5, y = 6 in let z = x + y in z" "11";
  t "m9" "let x = 5, y = 6 in let z = let a = x + y in a in z" "11";

  t "m10" "5 - 5" "0";
  t "m11" "let x = 5 in 5 * x" "25";
  
  (*te "unbound" "if 1: x else: 2" "Unbound var x at unbound, 1:6-1:7";*)

  ]
;;


let () =
  run_test_tt_main suite
;;
