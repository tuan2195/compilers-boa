(* Abstract syntax of (a small subset of) x86 assembly instructions *)
let word_size = 4
;;
       

type reg =
  | EAX
  | ESP

type arg =
  | Const of int
  | Reg of reg
  | RegOffset of int * reg (* int is # words of offset *)

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ILabel of string
  | ICmp of arg * arg
  | IJne of string
  | IJe of string
  | IJmp of string
  | IRet

type prim1 =
  | Add1
  | Sub1

type prim2 =
  | Plus
  | Minus
  | Times

type 'a bind = (string * 'a expr * 'a)

and 'a expr =
  | ELet of 'a bind list * 'a expr * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | ENumber of int * 'a
  | EId of string * 'a
