(*s: bytecomp/bytegen.ml *)
(*s: copyright header *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header *)

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Asttypes
open Primitive
open Types
open Lambda
open Instruct

(*s: constant [[Bytegen.label_counter]] *)
(**** Label generation ****)

let label_counter = ref 0
(*e: constant [[Bytegen.label_counter]] *)

(*s: function [[Bytegen.new_label]] *)
let new_label () =
  incr label_counter; !label_counter
(*e: function [[Bytegen.new_label]] *)

(*s: constant [[Bytegen.empty_env]] *)
(**** Operations on compilation environments. ****)

let empty_env =
  { ce_stack = Ident.empty; ce_heap = Ident.empty }
(*e: constant [[Bytegen.empty_env]] *)

(*s: function [[Bytegen.add_var]] *)
(* Add a stack-allocated variable *)

let add_var id pos env =
  { ce_stack = Ident.add id pos env.ce_stack;
    ce_heap = env.ce_heap }
(*e: function [[Bytegen.add_var]] *)

(**** Examination of the continuation ****)

(*s: constant [[Bytegen.label_code]] *)
(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
    Kbranch lbl :: _ as cont -> (lbl, cont)
  | Klabel lbl :: _ as cont -> (lbl, cont)
  | cont -> let lbl = new_label() in (lbl, Klabel lbl :: cont)
(*e: constant [[Bytegen.label_code]] *)

(*s: function [[Bytegen.make_branch]] *)
(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let make_branch cont =
  match cont with
    (Kbranch _ as branch) :: _ -> (branch, cont)
  | (Kreturn _ as return) :: _ -> (return, cont)
  | Kraise :: _ -> (Kraise, cont)
  | Klabel lbl :: _ -> (Kbranch lbl, cont)
  | _ -> let lbl = new_label() in (Kbranch lbl, Klabel lbl :: cont)
(*e: function [[Bytegen.make_branch]] *)

(*s: constant [[Bytegen.discard_dead_code]] *)
(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
    [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont
(*e: constant [[Bytegen.discard_dead_code]] *)

(*s: constant [[Bytegen.is_tailcall]] *)
(* Check if we're in tailcall position *)

let rec is_tailcall = function
    Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false
(*e: constant [[Bytegen.is_tailcall]] *)

(*s: function [[Bytegen.add_pop]] *)
(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0 then cont else
    match cont with
      Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn(n + m) :: cont
    | Kraise :: _ -> cont
    | _ -> Kpop n :: cont
(*e: function [[Bytegen.add_pop]] *)

(*s: constant [[Bytegen.add_const_unit]] *)
(* Add the constant "unit" in front of a continuation *)

let add_const_unit = function
    (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const_unit :: cont
(*e: constant [[Bytegen.add_const_unit]] *)

(**** Auxiliary for compiling "let rec" ****)

module IdentSet = Set

(*s: constant [[Bytegen.size_of_lambda]] *)
let rec size_of_lambda = function
  | Lfunction(kind, params, body) as funct ->
      1 + IdentSet.cardinal(free_variables funct)
  | Lprim(Pmakeblock(tag, mut), args) -> List.length args
  | Lprim(Pmakearray kind, args) -> List.length args
  | Llet(str, id, arg, body) -> size_of_lambda body
  | Lletrec(bindings, body) -> size_of_lambda body
  | Levent (lam, _) -> size_of_lambda lam
  | _ -> fatal_error "Bytegen.size_of_lambda"
(*e: constant [[Bytegen.size_of_lambda]] *)

(*s: function [[Bytegen.copy_event]] *)
(**** Merging consecutive events ****)

let copy_event ev kind info repr =
  { ev_pos = 0;                   (* patched in emitcode *)
    ev_module = ev.ev_module;
    ev_char = ev.ev_char;
    ev_kind = kind;
    ev_info = info;
    ev_typenv = ev.ev_typenv;
    ev_compenv = ev.ev_compenv;
    ev_stacksize = ev.ev_stacksize;
    ev_repr = repr }
(*e: function [[Bytegen.copy_event]] *)

(*s: function [[Bytegen.merge_infos]] *)
let merge_infos ev ev' =
  match ev.ev_info, ev'.ev_info with
    Event_other, info -> info
  | info, Event_other -> info
  | _                 -> fatal_error "Bytegen.merge_infos"
(*e: function [[Bytegen.merge_infos]] *)

(*s: function [[Bytegen.merge_repr]] *)
let merge_repr ev ev' =
  match ev.ev_repr, ev'.ev_repr with
    Event_none, x -> x
  | x, Event_none -> x
  | Event_parent r, Event_child r' when r == r' && !r = 1 -> Event_none
  | _, _          -> fatal_error "Bytegen.merge_repr"
(*e: function [[Bytegen.merge_repr]] *)

(*s: function [[Bytegen.merge_events]] *)
let merge_events ev ev' =
  let (maj, min) =
    match ev.ev_kind, ev'.ev_kind with
    (* Discard pseudo-events *)    
      Event_pseudo,  _                              -> ev', ev
    | _,             Event_pseudo                   -> ev,  ev'
    (* Keep following event, supposedly more informative *)
    | Event_before,  (Event_after _ | Event_before) -> ev',  ev
    (* Discard following events, supposedly less informative *)
    | Event_after _, (Event_after _ | Event_before) -> ev, ev'
  in
  copy_event maj maj.ev_kind (merge_infos maj min) (merge_repr maj min)
(*e: function [[Bytegen.merge_events]] *)

(*s: function [[Bytegen.weaken_event]] *)
let weaken_event ev cont =
  match ev.ev_kind with
    Event_after _ ->
      begin match cont with
        Kpush :: Kevent ({ev_repr = Event_none} as ev') :: c ->
          begin match ev.ev_info with
            Event_return _ ->
              (* Weaken event *)
              let repr = ref 1 in
              let ev =
                copy_event ev Event_pseudo ev.ev_info (Event_parent repr)
              and ev' =
                copy_event ev' ev'.ev_kind ev'.ev_info (Event_child repr)
              in
              Kevent ev :: Kpush :: Kevent ev' :: c
          | _ ->
              (* Only keep following event, equivalent *)
              cont
          end
      | _ ->
          Kevent ev :: cont
      end
  | _ ->
      Kevent ev :: cont
(*e: function [[Bytegen.weaken_event]] *)
  
(*s: function [[Bytegen.add_event]] *)
let add_event ev =
  function
    Kevent ev' :: cont -> weaken_event (merge_events ev ev') cont
  | cont               -> weaken_event ev cont
(*e: function [[Bytegen.add_event]] *)

(**** Compilation of a lambda expression ****)

(* The label to which Lstaticfail branches, and the stack size at that point.*)

let lbl_staticfail = ref 0
and sz_staticfail = ref 0

(*s: constant [[Bytegen.functions_to_compile]] *)
(* Function bodies that remain to be compiled *)

let functions_to_compile  =
(*e: constant [[Bytegen.functions_to_compile]] *)
  (Stack.create () : (Ident.t list * lambda * label * Ident.t list) Stack.t)

(*s: constant [[Bytegen.compunit_name]] *)
(* Name of current compilation unit (for debugging events) *)

let compunit_name = ref ""
(*e: constant [[Bytegen.compunit_name]] *)

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

let rec comp_expr env exp sz cont =
  match exp with
    Lvar id ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        Kacc(sz - pos) :: cont
      with Not_found ->
      try
        let pos = Ident.find_same id env.ce_heap in
        Kenvacc(pos) :: cont
      with Not_found ->
        Ident.print id; print_newline();
        fatal_error "Bytegen.comp_expr: var"
      end
  | Lconst cst ->
      Kconst cst :: cont
  | Lapply(func, args) ->
      let nargs = List.length args in
      if is_tailcall cont then
        comp_args env args sz
          (Kpush :: comp_expr env func (sz + nargs)
            (Kappterm(nargs, sz + nargs) :: discard_dead_code cont))
      else
        if nargs < 4 then
          comp_args env args sz
            (Kpush :: comp_expr env func (sz + nargs) (Kapply nargs :: cont))
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args env args (sz + 3)
            (Kpush :: comp_expr env func (sz + 3 + nargs)
                      (Kapply nargs :: cont1))
        end
  | Lfunction(kind, params, body) -> (* assume kind = Curried *)
      let lbl = new_label() in
      let fv = IdentSet.elements(free_variables exp) in
      Stack.push (params, body, lbl, fv) functions_to_compile;
      comp_args env (List.map (fun n -> Lvar n) fv) sz
        (Kclosure(lbl, List.length fv) :: cont)
  | Llet(str, id, arg, body) ->
      comp_expr env arg sz
        (Kpush :: comp_expr (add_var id (sz+1) env) body (sz+1)
          (add_pop 1 cont))
  | Lletrec(([id, Lfunction(kind, params, funct_body)] as decl), let_body) ->
      let lbl = new_label() in
      let fv =
        IdentSet.elements (free_variables (Lletrec(decl, lambda_unit))) in
      Stack.push (params, funct_body, lbl, id :: fv) functions_to_compile;
      comp_args env (List.map (fun n -> Lvar n) fv) sz
        (Kclosurerec(lbl, List.length fv) :: Kpush ::
          (comp_expr (add_var id (sz+1) env) let_body (sz+1)
                     (add_pop 1 cont)))
  | Lletrec(decl, body) ->
      let ndecl = List.length decl in
      let decl_size =
        List.map (fun (id, exp) -> (id, exp, size_of_lambda exp)) decl in
      let rec comp_decl new_env sz i = function
          [] ->
            comp_expr new_env body sz (add_pop ndecl cont)
        | (id, exp, blocksize) :: rem ->
            comp_expr new_env exp sz
              (Kpush :: Kacc i :: Kupdate blocksize ::
               comp_decl new_env sz (i-1) rem) in
      let rec comp_init new_env sz = function
          [] ->
            comp_decl new_env sz ndecl decl_size
        | (id, exp, blocksize) :: rem ->
            Kdummy blocksize :: Kpush ::
            comp_init (add_var id (sz+1) new_env) (sz+1) rem in
      comp_init env sz decl_size
  | Lprim(Pidentity, [arg]) ->
      comp_expr env arg sz cont
  | Lprim(Pnot, [arg]) ->
      let newcont =
        match cont with
          Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
        | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
        | _ -> Kboolnot :: cont in
      comp_expr env arg sz newcont
  | Lprim(Psequand, [exp1; exp2]) ->
      begin match cont with
        Kbranchifnot lbl :: _ ->
          comp_expr env exp1 sz (Kbranchifnot lbl ::
            comp_expr env exp2 sz cont)
      | Kbranchif lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr env exp1 sz (Kbranchifnot lbl2 ::
            comp_expr env exp2 sz (Kbranchif lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr env exp1 sz (Kstrictbranchifnot lbl ::
            comp_expr env exp2 sz cont1)
      end
  | Lprim(Psequor, [exp1; exp2]) ->
      begin match cont with
        Kbranchif lbl :: _ ->
          comp_expr env exp1 sz (Kbranchif lbl ::
            comp_expr env exp2 sz cont)
      | Kbranchifnot lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr env exp1 sz (Kbranchif lbl2 ::
            comp_expr env exp2 sz (Kbranchifnot lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr env exp1 sz (Kstrictbranchif lbl ::
            comp_expr env exp2 sz cont1)
      end
  | Lprim(Praise, [arg]) ->
      comp_expr env arg sz (Kraise :: discard_dead_code cont)
  | Lprim((Paddint | Psubint as prim), [arg; Lconst(Const_base(Const_int n))])
    when n >= immed_min & n <= immed_max ->
      let ofs = if prim == Paddint then n else -n in
      comp_expr env arg sz (Koffsetint ofs :: cont)
  | Lprim(p, args) ->
      let instr =
        match p with
          Pgetglobal id -> Kgetglobal id
        | Psetglobal id -> Ksetglobal id
        | Pintcomp cmp -> Kintcomp cmp
        | Pmakeblock(tag, mut) -> Kmakeblock(List.length args, tag)
        | Pfield n -> Kgetfield n
        | Psetfield(n, ptr) -> Ksetfield n
        | Pfloatfield n -> Kgetfield n
        | Psetfloatfield n -> Ksetfield n
        | Pccall p -> Kccall(p.prim_name, p.prim_arity)
        | Pnegint -> Knegint
        | Paddint -> Kaddint
        | Psubint -> Ksubint
        | Pmulint -> Kmulint
        | Pdivint -> Kdivint
        | Pmodint -> Kmodint
        | Pandint -> Kandint
        | Porint -> Korint
        | Pxorint -> Kxorint
        | Plslint -> Klslint
        | Plsrint -> Klsrint
        | Pasrint -> Kasrint
        | Poffsetint n -> Koffsetint n
        | Poffsetref n -> Koffsetref n
        | Pintoffloat -> Kccall("int_of_float", 1)
        | Pfloatofint -> Kccall("float_of_int", 1)
        | Pnegfloat -> Kccall("neg_float", 1)
        | Pabsfloat -> Kccall("abs_float", 1)
        | Paddfloat -> Kccall("add_float", 2)
        | Psubfloat -> Kccall("sub_float", 2)
        | Pmulfloat -> Kccall("mul_float", 2)
        | Pdivfloat -> Kccall("div_float", 2)
        | Pfloatcomp Ceq -> Kccall("eq_float", 2)
        | Pfloatcomp Cneq -> Kccall("neq_float", 2)
        | Pfloatcomp Clt -> Kccall("lt_float", 2)
        | Pfloatcomp Cgt -> Kccall("gt_float", 2)
        | Pfloatcomp Cle -> Kccall("le_float", 2)
        | Pfloatcomp Cge -> Kccall("ge_float", 2)
        | Pstringlength -> Kccall("ml_string_length", 1)
        | Pstringrefs -> Kccall("string_get", 2)
        | Pstringsets -> Kccall("string_set", 3)
        | Pstringrefu -> Kgetstringchar
        | Pstringsetu -> Ksetstringchar
        | Pmakearray kind -> Kmakeblock(List.length args, 0)
        | Parraylength kind -> Kvectlength
        | Parrayrefs kind -> Kccall("array_get", 2)
        | Parraysets kind -> Kccall("array_set", 3)
        | Parrayrefu kind -> Kgetvectitem
        | Parraysetu kind -> Ksetvectitem
        | Pisint -> Kisint
        | Pbittest -> Kccall("bitvect_test", 2)
        | _ -> fatal_error "Bytegen.comp_expr: prim" in
      comp_args env args sz (instr :: cont)
  | Lcatch(body, Lstaticfail) ->
      comp_expr env body sz cont
  | Lcatch(body, handler) ->
      let (branch1, cont1) = make_branch cont in
      let (lbl_handler, cont2) = label_code (comp_expr env handler sz cont1) in
      let saved_lbl_staticfail = !lbl_staticfail
      and saved_sz_staticfail = !sz_staticfail in
      lbl_staticfail := lbl_handler;
      sz_staticfail := sz;
      let cont3 = comp_expr env body sz (branch1 :: cont2) in
      lbl_staticfail := saved_lbl_staticfail;
      sz_staticfail := saved_sz_staticfail;
      cont3
  | Lstaticfail ->
      add_pop (sz - !sz_staticfail)
              (Kbranch !lbl_staticfail :: discard_dead_code cont)
  | Ltrywith(body, id, handler) ->
      let (branch1, cont1) = make_branch cont in
      let lbl_handler = new_label() in
      Kpushtrap lbl_handler :: 
        comp_expr env body (sz+4) (Kpoptrap :: branch1 :: 
          Klabel lbl_handler :: Kpush ::
            comp_expr (add_var id (sz+1) env) handler (sz+1) (add_pop 1 cont1))
  | Lifthenelse(cond, ifso, ifnot) ->
      comp_binary_test env cond ifso ifnot sz cont
  | Lsequence(exp1, exp2) ->
      comp_expr env exp1 sz (comp_expr env exp2 sz cont)
  | Lwhile(cond, body) ->
      let lbl_loop = new_label() in
      let lbl_test = new_label() in
      Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals ::
        comp_expr env body sz
          (Klabel lbl_test ::
            comp_expr env cond sz (Kbranchif lbl_loop :: add_const_unit cont))
  | Lfor(param, start, stop, dir, body) ->
      let lbl_loop = new_label() in
      let lbl_test = new_label() in
      let offset = match dir with Upto -> 1 | Downto -> -1 in
      let comp = match dir with Upto -> Cle | Downto -> Cge in
      comp_expr env start sz
        (Kpush :: comp_expr env stop (sz+1)
          (Kpush :: Kbranch lbl_test ::
           Klabel lbl_loop :: Kcheck_signals ::
           comp_expr (add_var param (sz+1) env) body (sz+2)
             (Kacc 1 :: Koffsetint offset :: Kassign 1 ::
              Klabel lbl_test ::
              Kacc 0 :: Kpush :: Kacc 2 :: Kintcomp comp ::
              Kbranchif lbl_loop ::
              add_const_unit (add_pop 2 cont))))
  | Lswitch(arg, sw) ->
      let (branch, cont1) = make_branch cont in
      let c = ref (discard_dead_code cont1) in
      let act_consts = Array.create sw.sw_numconsts Lstaticfail in
      List.iter (fun (n, act) -> act_consts.(n) <- act) sw.sw_consts;
      let act_blocks = Array.create sw.sw_numblocks Lstaticfail in
      List.iter (fun (n, act) -> act_blocks.(n) <- act) sw.sw_blocks;
      let lbl_consts = Array.create sw.sw_numconsts 0 in
      let lbl_blocks = Array.create sw.sw_numblocks 0 in
      for i = sw.sw_numblocks - 1 downto 0 do
        let (lbl, c1) =
          label_code(comp_expr env act_blocks.(i) sz (branch :: !c)) in
        lbl_blocks.(i) <- lbl;
        c := discard_dead_code c1
      done;
      for i = sw.sw_numconsts - 1 downto 0 do
        let (lbl, c1) =
          label_code(comp_expr env act_consts.(i) sz (branch :: !c)) in
        lbl_consts.(i) <- lbl;
        c := discard_dead_code c1
      done;
      if sw.sw_checked then c := comp_expr env Lstaticfail sz !c;        
      comp_expr env arg sz (Kswitch(lbl_consts, lbl_blocks) :: !c)
  | Lassign(id, expr) ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        comp_expr env expr sz (Kassign(sz - pos) :: cont)
      with Not_found ->
        fatal_error "Bytegen.comp_expr: assign"
      end
  | Levent(lam, lev) ->
      let event kind info =
        { ev_pos = 0;                   (* patched in emitcode *)
          ev_module = !compunit_name;
          ev_char = lev.lev_loc;
          ev_kind = kind;
          ev_info = info;
          ev_typenv = lev.lev_env;
          ev_compenv = env;
          ev_stacksize = sz;
          ev_repr =
            begin match lev.lev_repr with
              None ->
                Event_none
            | Some ({contents = 1} as repr) when lev.lev_kind = Lev_function ->
                Event_child repr
            | Some ({contents = 1} as repr) ->
                Event_parent repr
            | Some repr when lev.lev_kind = Lev_function ->
                Event_parent repr
            | Some repr ->
                Event_child repr
            end }
      in
      begin match lev.lev_kind with
        Lev_before ->
          let c = comp_expr env lam sz cont in
          let ev = event Event_before Event_other in
          add_event ev c
      | Lev_function ->
          let c = comp_expr env lam sz cont in
          let ev = event Event_pseudo Event_function in
          add_event ev c
      | Lev_after _ when is_tailcall cont -> (* don't destroy tail call opt *)
          comp_expr env lam sz cont
      | Lev_after ty ->
          let info =
            match lam with
              Lapply(_, args)   -> Event_return (List.length args)
            | _                 -> Event_other
          in
          let ev = event (Event_after ty) info in
          let cont1 = add_event ev cont in
          comp_expr env lam sz cont1
      end

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args env argl sz cont =
  comp_expr_list env (List.rev argl) sz cont

and comp_expr_list env exprl sz cont =
  match exprl with
    [] -> cont
  | [exp] -> comp_expr env exp sz cont
  | exp :: rem ->
      comp_expr env exp sz (Kpush :: comp_expr_list env rem (sz+1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Lconst const_unit then begin
      let (lbl_end, cont1) = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr env ifso sz cont1
    end else
    if ifso = Lstaticfail & sz = !sz_staticfail then
      Kbranchif !lbl_staticfail :: comp_expr env ifnot sz cont
    else
    if ifnot = Lstaticfail & sz = !sz_staticfail then
      Kbranchifnot !lbl_staticfail :: comp_expr env ifso sz cont
    else begin
      let (branch_end, cont1) = make_branch cont in
      let (lbl_not, cont2) = label_code(comp_expr env ifnot sz cont1) in
      Kbranchifnot lbl_not :: comp_expr env ifso sz (branch_end :: cont2)
    end in
  comp_expr env cond sz cont_cond

(*s: function [[Bytegen.comp_function]] *)
(**** Compilation of functions ****)

let comp_function (params, fun_body, entry_lbl, free_vars) cont =
  let arity = List.length params in
  let rec pos_args pos delta = function
      [] -> Ident.empty
    | id :: rem -> Ident.add id pos (pos_args (pos+delta) delta rem) in
  let env =
    { ce_stack = pos_args arity (-1) params;
      ce_heap = pos_args 0 1 free_vars } in
  let cont1 =
    comp_expr env fun_body arity (Kreturn arity :: cont) in
  if arity > 1 then
    Krestart :: Klabel entry_lbl :: Kgrab(arity - 1) :: cont1
  else
    Klabel entry_lbl :: cont1
(*e: function [[Bytegen.comp_function]] *)

(*s: function [[Bytegen.comp_remainder]] *)
let comp_remainder cont =
  let c = ref cont in
  begin try
    while true do
      c := comp_function (Stack.pop functions_to_compile) !c
    done
  with Stack.Empty ->
    ()
  end;
  !c
(*e: function [[Bytegen.comp_remainder]] *)

(*s: function [[Bytegen.compile_implementation]] *)
(**** Compilation of a lambda phrase ****)

let compile_implementation modulename expr =
  Stack.clear functions_to_compile;
  label_counter := 0;
  lbl_staticfail := 0;
  sz_staticfail := 0;
  compunit_name := modulename;
  let init_code = comp_expr empty_env expr 0 [] in
  if Stack.length functions_to_compile > 0 then begin
    let lbl_init = new_label() in
    Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code)
  end else
    init_code
(*e: function [[Bytegen.compile_implementation]] *)

(*s: function [[Bytegen.compile_phrase]] *)
let compile_phrase expr =
  Stack.clear functions_to_compile;
  label_counter := 0;
  lbl_staticfail := 0;
  sz_staticfail := 0;
  let init_code = comp_expr empty_env expr 1 [Kreturn 1] in
  let fun_code = comp_remainder [] in
  (init_code, fun_code)
(*e: function [[Bytegen.compile_phrase]] *)

(*e: bytecomp/bytegen.ml *)
