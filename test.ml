open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Types

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

  tanf "forty_one_anf"
       (ENumber(41, ()))
       forty_one_a;

  (*tanf "forty_one_anf"*)
       (*(EPrim2(Times, ENumber(42,()), ENumber(24,()), ()))*)
       (*forty_one_a;*)

  (*tanf "prim1_anf"*)
       (*(EPrim1(Sub1, EPrim1(Add1, ENumber(77,()), ()), ()))*)
       (*(ELet(["unary_1", EPrim1(Sub1, ENumber(55, ()), ()), ()],*)
             (*EId("unary_1", ()),*)
             (*()));*)

  (*tstr "prim1_anf"*)
       (*(EPrim1(Sub1, EPrim1(Add1, ENumber(77,()), ()), ()))*)
       (*("test");*)

  (*tstr "if1"*)
        (*(EIf(ENumber(55, ()),*)
             (*EPrim2(Times, ENumber(12, ()), ENumber(24, ()), ()),*)
             (*EPrim1(Sub1, EPrim1(Add1, ENumber(77,()), ()), ()), ()))*)
        (*("test");*)

  (*ta "forty_one_run_anf" (tag forty_one_a) "41";*)

  (*t "forty_one" forty_one "41";*)


  (*t "test" test_prog "3";*)

    (*(* Some useful if tests to start you off *)*)

  t "if1" "if 5: 4 else: 2" "4";
  t "if2" "if 0: 4 else: 2" "2";

  ]
;;


let () =
  run_test_tt_main suite
;;
