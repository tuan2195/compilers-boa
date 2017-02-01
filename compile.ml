open Printf
open Types
open Pretty

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EId _ -> true
  | _ -> false
;;

(* This function should encapsulate the binding-error checking from Adder *)
exception BindingError of string
let rec check_scope (e : (Lexing.position * Lexing.position) expr) : unit =
  let rec helper (e : 'a expr) (bound : string list) : unit =
    match e with
    | ENumber _ -> ()
    | EId (s, pos) ->
        if List.mem s bound then ()
        else raise (BindingError(sprintf "Unbound var %s at %s" s (string_of_pos pos)))
    | EPrim1 (_, e, _) ->
        helper e bound
    | EPrim2 (_, e1, e2, _) ->
        helper e1 bound ;
        helper e2 bound
    | EIf (cond, thn, els, _) ->
        helper cond bound ;
        helper thn bound ;
        helper els bound
    | ELet (binds, body, _) ->
        let newBound = List.fold_left
            (fun ls (str, exp, _) -> helper exp ls; str::ls) bound binds
        in helper body newBound
  in helper e []

type tag = int
(* PROBLEM 2 *)
(* This function assigns a unique tag to every subexpression and let binding *)
let tag (e : 'a expr) : tag expr =
    let rec tagger (e : 'a expr) (t : tag) : (tag expr * tag) =
        let rec bindsTagger (bls : 'a bind list) (t : tag) : (tag bind list * tag) =
            match bls with
            | [] -> ([], t)
            | b::rest ->
                let (str, bindsExp, _) = b in
                let (newBindsExp, nextTag) = tagger bindsExp t in
                let newBinds = (str, newBindsExp, nextTag) in
                let (newRestBindsLs, nextTagRec) = bindsTagger rest (nextTag+1) in
                (newBinds::newRestBindsLs, nextTagRec)
        in match e with
        | ELet (bindsLs, exp, _) ->
            let (newBindsLs, nextTag1) = bindsTagger bindsLs t in
            let (newExp, nextTag2) = tagger exp nextTag1 in
            (ELet (newBindsLs, newExp, nextTag2), nextTag2+1)
        | EPrim1 (prim, exp, _) ->
            let (newExp, nextTag) = tagger exp t in
            (EPrim1(prim, newExp, nextTag), nextTag+1)
        | EPrim2 (prim, exp1, exp2, _) ->
            let (newExp1, nextTag1) = tagger exp1 t in
            let (newExp2, nextTag2) = tagger exp2 nextTag1 in
            (EPrim2(prim, newExp1, newExp2, nextTag2), nextTag2+1)
        | EIf (exp1, exp2, exp3, _) ->
            let (newExp1, nextTag1) = tagger exp1 t in
            let (newExp2, nextTag2) = tagger exp2 nextTag1 in
            let (newExp3, nextTag3) = tagger exp3 nextTag2 in
            (EIf(newExp1, newExp2, newExp3, nextTag3), nextTag3+1)
        | ENumber (x, _) ->
            (ENumber(x, t), t+1)
        | EId (x, _) ->
            (EId(x, t), t+1)
    in let (newExp, _) = tagger e 0 in newExp
;;

(* This function removes all tags, and replaces them with the unit value.
   This might be convenient for testing, when you don't care about the tag info. *)
let rec untag (e : 'a expr) : unit expr =
  match e with
  | EId(x, _) -> EId(x, ())
  | ENumber(n, _) -> ENumber(n, ())
  | EPrim1(op, e, _) ->
     EPrim1(op, untag e, ())
  | EPrim2(op, e1, e2, _) ->
     EPrim2(op, untag e1, untag e2, ())
  | ELet(binds, body, _) ->
     ELet(List.map(fun (x, b, _) -> (x, untag b, ())) binds, untag body, ())
  | EIf(cond, thn, els, _) ->
     EIf(untag cond, untag thn, untag els, ())

(* PROBLEM 3 & 4 *)
(* This function converts a tagged expression into an untagged expression in A-normal form *)
let gensym (e : tag expr) : string =
    match e with
    | EId(_, t) ->
        sprintf "Id_%d" t
    | ENumber(_, t) ->
        sprintf "Num_%d" t
    | EPrim1(_, _, t) ->
        sprintf "Prim1_%d" t
    | EPrim2(_, _, _, t) ->
        sprintf "Prim2_%d" t
    | ELet(_, _, t) ->
        sprintf "Let_%d" t
    | EIf(_, _, _, t) ->
        sprintf "If_%d" t

let anf (e : tag expr) : unit expr =
    let addUnit = (fun (str, expr) -> (str, expr, ())) in
    let rec helpBinds (ls : 'a bind list) : ((string * unit expr) list) =
        match ls with
        | [] -> []
        | bind::rest ->
            let (_, exp, tag) = bind in
            let (ans, ctx) = helpC exp in
            let name = gensym exp in
            ((name, ans)::ctx)@helpBinds rest
    and helpC (e : tag expr) : (unit expr * (string * unit expr) list) =
        match e with
        | EPrim1(op, e, _) ->
            let (ans, ctx) = helpI e in
            (EPrim1(op, ans, ()), ctx)
        | EPrim2(op, e1, e2, _) ->
            let (ans1, ctx1) = helpI e1 in
            let (ans2, ctx2) = helpI e2 in
            (EPrim2(op, ans1, ans2, ()), ctx1@ctx2)
        | ELet(binds, exp, _) ->
            let bindsCtx = helpBinds binds in
            let (expAns, expCtx) = helpC exp in
            (ELet(List.map addUnit expCtx, expAns, ()), bindsCtx)
        | EIf(cond, thn, els, _) ->
            let (ansCond, ctxCond) = helpI cond in
            let (ansThen, ctxThen) = helpC thn in
            let (ansElse, ctxElse) = helpC els in
            (EIf(ELet(List.map addUnit ctxCond, ansCond, ()),
                 ELet(List.map addUnit ctxThen, ansThen, ()),
                 ELet(List.map addUnit ctxElse, ansElse, ()),
                 ()), [])
        | _ -> helpI e
    and helpI (e : tag expr) : (unit expr * (string * unit expr) list) =
        match e with
        | EId(x, _) ->
            (EId(x, ()), [])
        | ENumber(x, _) ->
            (ENumber(x, ()), [])
        | EPrim1(op, e, _) ->
            let (ans, ctx) = helpI e in
            let name = gensym e in
            (EId(name, ()), ctx@[(name, EPrim1(op, ans, ()))])
        | EPrim2(op, e1, e2, _) ->
            let (ans1, ctx1) = helpI e1 in
            let (ans2, ctx2) = helpI e2 in
            let name = gensym e in
            (EId(name, ()), ctx1@ctx2@[(name, EPrim2(op, ans1, ans2, ()))])
        | ELet(binds, exp, _) ->
            let bindsCtx = helpBinds binds in
            let (expAns, expCtx) = helpC exp in
            let name = gensym e in
            (EId(name, ()), bindsCtx@expCtx@[(name, expAns)])
        | EIf(cond, thn, els, _) ->
            let (ansCond, ctxCond) = helpI cond in
            let (ansThen, ctxThen) = helpC thn in
            let (ansElse, ctxElse) = helpC els in
            let name = gensym e in
            (EId(name, ()), [(name, EIf( ELet(List.map addUnit ctxCond, ansCond, ()),
                                         ELet(List.map addUnit ctxThen, ansThen, ()),
                                         ELet(List.map addUnit ctxElse, ansElse, ()),
                                         ()))])
    in if is_anf e
        then untag e
    else
        let (ans, ctx) = helpC e in
        ELet(List.map addUnit ctx, ans, ())
;;


(* Helper functions *)
let r_to_asm (r : reg) : string =
  match r with
  | EAX -> "eax"
  | ESP -> "esp"

let arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%d" n
  | Reg(r) -> r_to_asm r
  | RegOffset(n, r) ->
     if n >= 0 then
       sprintf "[%s+%d]" (r_to_asm r) (word_size * n)
     else
       sprintf "[%s-%d]" (r_to_asm r) (-1 * word_size * n)
let i_to_asm (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
     sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd(dest, arg) ->
     sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm arg)
  | ISub(dest, arg) ->
     sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm arg)
  | IMul(dest, arg) ->
     sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm arg)
  | ILabel(label) ->
     sprintf "%s:" label
  | ICmp(arg1, arg2) ->
     sprintf "  cmp %s, %s" (arg_to_asm arg1) (arg_to_asm arg2)
  | IJne(label) ->
     sprintf "  jne %s" label
  | IJe(label) ->
     sprintf "  je %s" label
  | IJmp(label) ->
     sprintf "  jmp %s" label
  | IRet ->
     "  ret"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let rec find ls x =
  match ls with
  | [] -> failwith (sprintf "Name %s not found" x)
  | (y,v)::rest ->
     if y = x then v else find rest x

(* PROBLEM 5 *)
(* This function actually compiles the tagged ANF expression into assembly *)
(* The si parameter should be used to indicate the next available stack index for use by Lets *)
(* The env parameter associates identifier names to stack indices *)
let rec optimize (ls : instruction list) =
    match ls with
    | [] -> []
    | (IMov(RegOffset(o1, ESP), Reg(EAX)))::(IMov(Reg(EAX), RegOffset(o2, ESP)))::rest ->
        if o1 = o2 then optimize rest
        else (List.nth ls 0)::(List.nth ls 1)::optimize rest
    | what::rest ->
        what::optimize rest

let rec compile_expr (e : tag expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | ENumber(n, _) -> [ IMov(Reg(EAX), compile_imm e env) ]
  | EId(x, _) -> [ IMov(Reg(EAX), compile_imm e env) ]
  | EPrim1(op, e, _) ->
     let e_reg = compile_imm e env in
     begin match op with
     | Add1 ->
       [ IMov(Reg(EAX), e_reg);
         IAdd(Reg(EAX), Const(1)) ]
     | Sub1 ->
       [ IMov(Reg(EAX), e_reg);
         IAdd(Reg(EAX), Const(~-1)) ]
     end
  | EPrim2(op, left, right, _) ->
     let regLeft = compile_imm left env in
     let regRight = compile_imm right env in
     begin match op with
     | Plus ->
       [ IMov(Reg(EAX), regLeft);
         IAdd(Reg(EAX), regRight); ]
     | Minus ->
       [ IMov(Reg(EAX), regLeft);
         ISub(Reg(EAX), regRight); ]
     | Times ->
       [ IMov(Reg(EAX), regLeft);
         IMul(Reg(EAX), regRight); ]
     end
  | EIf(cond, thn, els, tag) ->
     let labelElse = sprintf "if_false_%d" tag in
     let labelDone = sprintf "done_%d" tag in
     ((compile_expr cond si env) @
     [   ICmp(Reg(EAX), Const(0));
         IJe(labelElse);   ] @
     (compile_expr thn si env) @
     [   IJmp(labelDone);
         ILabel(labelElse);   ] @
     (compile_expr els si env) @
     [   ILabel(labelDone);   ])
  | ELet(bindsLs, body, _) ->
     let (instLs, newSi, newEnv) = List.fold_left
     (fun (instLs, si, env) (bind) ->
         let (id, exp, _) = bind in
         let newInstLs = compile_expr exp si env in
         let newEnv = [(id, si)]@env in
         ((instLs @ newInstLs @ [ IMov(RegOffset(~-si, ESP), Reg(EAX)) ]), si+1, newEnv)
     )
     ([], si, env)
     (bindsLs) in
     (instLs @ compile_expr body newSi newEnv)
and compile_imm e env =
  match e with
  | ENumber(n, _) -> Const(n)
  | EId(x, _) -> RegOffset(~-(find env x), ESP)
  | _ -> failwith "Impossible: not an immediate"


let compile_anf_to_string anfed =
  let prelude =
    "section .text
global our_code_starts_here
our_code_starts_here:" in
  let compiled = (compile_expr anfed 1 []) in
  let compiled = optimize compiled in
  let compiled = optimize compiled in
  let as_assembly_string = (to_asm (compiled @ [IRet])) in
  (*printf "%s" as_assembly_string;*)
  sprintf "%s%s\n" prelude as_assembly_string


let compile_to_string prog =
  check_scope prog;
  let tagged : tag expr = tag prog in
  let anfed : tag expr = tag (anf tagged) in
  (* printf "Prog:\n%s\n" (ast_of_expr prog); *)
  (* printf "Tagged:\n%s\n" (format_expr tagged string_of_int); *)
  (* printf "ANFed/tagged:\n%s\n" (format_expr anfed string_of_int); *)
  compile_anf_to_string anfed

