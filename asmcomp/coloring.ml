(*s: asmcomp/coloring.ml *)
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

(* Register allocation by coloring of the interference graph *)

open Reg

(*s: function [[Coloring.allocate_spilled]] *)
(* Preallocation of spilled registers in the stack. *)

let allocate_spilled reg =
  if reg.spill then begin
    let cl = Proc.register_class reg in
    let nslots = Proc.num_stack_slots.(cl) in
    let conflict = Array.create nslots false in
    List.iter
      (fun r ->
        match r.loc with
          Stack(Local n) ->
            if Proc.register_class r = cl then conflict.(n) <- true
        | _ -> ())
      reg.interf;
    let slot = ref 0 in
    while !slot < nslots & conflict.(!slot) do incr slot done;
    reg.loc <- Stack(Local !slot);
    if !slot >= nslots then Proc.num_stack_slots.(cl) <- !slot + 1
  end
(*e: function [[Coloring.allocate_spilled]] *)

(*s: constant [[Coloring.unconstrained]] *)
(* Compute the degree (= number of neighbours of the same type)
   of each register, and split them in two sets:
   unconstrained (degree < number of available registers)
   and constrained (degree >= number of available registers).
   Spilled registers are ignored in the process. *)

let unconstrained = ref (*Reg.*)Set.empty
(*e: constant [[Coloring.unconstrained]] *)
(*s: constant [[Coloring.constrained]] *)
let constrained = ref (*Reg.*)Set.empty
(*e: constant [[Coloring.constrained]] *)

(*s: function [[Coloring.find_degree]] *)
let find_degree reg =
  if reg.spill then () else begin
    let cl = Proc.register_class reg in
    let avail_regs = Proc.num_available_registers.(cl) in
    if avail_regs = 0 then
      (* Don't bother computing the degree if there are no regs 
         in this class *)
      unconstrained := (*Reg.*)Set.add reg !unconstrained
    else begin
      let deg = ref 0 in
      List.iter
        (fun r -> if not r.spill & Proc.register_class r = cl then incr deg)
        reg.interf;
      reg.degree <- !deg;
      if !deg >= avail_regs
      then constrained := (*Reg.*)Set.add reg !constrained
      else unconstrained := (*Reg.*)Set.add reg !unconstrained
    end
  end
(*e: function [[Coloring.find_degree]] *)

(*s: function [[Coloring.remove_all_regs]] *)
(* Remove all registers one by one, unconstrained if possible, otherwise
   constrained with lowest spill cost. Return the list of registers removed
   in reverse order.
   The spill cost measure is [r.spill_cost / r.degree].
   [r.spill_cost] estimates the number of accesses to this register. *)

let rec remove_all_regs stack =
  if not ((*Reg.*)Set.is_empty !unconstrained) then begin
    (* Pick any unconstrained register *)
    let r = (*Reg.*)Set.choose !unconstrained in
    unconstrained := (*Reg.*)Set.remove r !unconstrained;
    remove_all_regs (r :: stack)
  end else
  if not ((*Reg.*)Set.is_empty !constrained) then begin
    (* Find a constrained reg with minimal cost *)
    let r = ref Reg.dummy in
    let min_degree = ref 0 and min_spill_cost = ref 1 in
      (* initially !min_spill_cost / !min_degree is +infty *)
    (*Reg.*)Set.iter
      (fun r2 ->
        (* if r2.spill_cost / r2.degree < !min_spill_cost / !min_degree *)
        if r2.spill_cost * !min_degree < !min_spill_cost * r2.degree
        then begin
          r := r2; min_degree := r2.degree; min_spill_cost := r2.spill_cost
        end)
      !constrained;
    constrained := (*Reg.*)Set.remove !r !constrained;
    remove_all_regs (!r :: stack)
  end else
    stack                             (* All regs have been removed *)
(*e: function [[Coloring.remove_all_regs]] *)

(*s: function [[Coloring.iter_preferred]] *)
(* Iterate over all registers preferred by the given register (transitively) *)

let iter_preferred f reg =
  let rec walk r w =
    if not r.visited then begin
      f r w;
      begin match r.prefer with
          [] -> ()
        | p  -> r.visited <- true;
                List.iter (fun (r1, w1) -> walk r1 (min w w1)) p;
                r.visited <- false
      end
    end in
  reg.visited <- true;
  List.iter (fun (r, w) -> walk r w) reg.prefer;
  reg.visited <- false
(*e: function [[Coloring.iter_preferred]] *)

(*s: constant [[Coloring.start_register]] *)
(* Where to start the search for a suitable register. 
   Used to introduce some "randomness" in the choice between registers
   with equal scores. This offers more opportunities for scheduling. *)

let start_register = Array.create Proc.num_register_classes 0
(*e: constant [[Coloring.start_register]] *)

(*s: function [[Coloring.assign_location]] *)
(* Assign a location to a register, the best we can *)

let assign_location reg =
  let cl = Proc.register_class reg in
  let first_reg = Proc.first_available_register.(cl) in
  let num_regs = Proc.num_available_registers.(cl) in
  let last_reg = first_reg + num_regs in
  let score = Array.create num_regs 0 in
  let best_score = ref (-1000000) and best_reg = ref (-1) in
  let start = start_register.(cl) in
  if num_regs > 0 then begin
    (* Favor the registers that have been assigned to pseudoregs for which
       we have a preference. If these pseudoregs have not been assigned
       already, avoid the registers with which they conflict. *)
    iter_preferred
      (fun r w ->
        match r.loc with
          Reg n -> if n >= first_reg & n < last_reg then
                     score.(n - first_reg) <- score.(n - first_reg) + w
        | Unknown ->
            List.iter
              (fun neighbour ->
                match neighbour.loc with
                  Reg n -> if n >= first_reg & n < last_reg then
                           score.(n - first_reg) <- score.(n - first_reg) - w
                | _ -> ())
              r.interf
        | _ -> ())
      reg;
    List.iter
      (fun neighbour ->
        (* Prohibit the registers that have been assigned
           to our neighbours *)
        begin match neighbour.loc with
          Reg n -> if n >= first_reg & n < last_reg then
                     score.(n - first_reg) <- (-1000000)
        | _ -> ()
        end;
        (* Avoid the registers that have been assigned to pseudoregs
           for which our neighbours have a preference *)
        iter_preferred
          (fun r w ->
            match r.loc with
              Reg n -> if n >= first_reg & n < last_reg then
                         score.(n - first_reg) <- score.(n - first_reg) - (w - 1)
                       (* w-1 to break the symmetry when two conflicting regs
                          have the same preference for a third reg. *)
            | _ -> ())
          neighbour)
      reg.interf;
    (* Pick the register with the best score *)
    for n = start to num_regs - 1 do
      if score.(n) > !best_score then begin
        best_score := score.(n);
        best_reg := n
      end
    done;
    for n = 0 to start - 1 do
      if score.(n) > !best_score then begin
        best_score := score.(n);
        best_reg := n
      end
    done
  end;
  (* Found a register? *)
  if !best_reg >= 0 then begin
    reg.loc <- Reg(first_reg + !best_reg);
    if Proc.rotate_registers then
      start_register.(cl) <- (if start+1 >= num_regs then 0 else start+1)
  end else begin
    (* Sorry, we must put the pseudoreg in a stack location *)
    (* First, check if we have a preference for an incoming location
       we do not conflict with. *)
    let best_score = ref 0 and best_incoming_loc = ref (-1) in
    List.iter
      (fun (r, w) ->
        match r.loc with
          Stack(Incoming n) ->
            if w > !best_score
             & List.for_all (fun neighbour -> neighbour.loc <> r.loc)
                            reg.interf
            then begin
              best_score := w;
              best_incoming_loc := n
            end
        | _ -> ())
      reg.prefer;
    if !best_incoming_loc >= 0 then
      reg.loc <- Stack(Incoming !best_incoming_loc)
    else begin
      (* Now, look for a location in the local area *)
      let nslots = Proc.num_stack_slots.(cl) in
      let score = Array.create nslots 0 in
      (* Compute the scores as for registers *)
      List.iter
        (fun (r, w) ->
          match r.loc with
            Stack(Local n) -> if Proc.register_class r = cl then
                              score.(n) <- score.(n) + w
          | Unknown ->
              List.iter
                (fun neighbour ->
                  match neighbour.loc with
                    Stack(Local n) ->
                      if Proc.register_class neighbour = cl
                      then score.(n) <- score.(n) - w
                  | _ -> ())
                r.interf
          | _ -> ())
        reg.prefer;
      List.iter
        (fun neighbour ->
          begin match neighbour.loc with
              Stack(Local n) ->
                if Proc.register_class neighbour = cl then
                score.(n) <- (-1000000)
          | _ -> ()
          end;
          List.iter
            (fun (r, w) ->
              match r.loc with
                Stack(Local n) -> if Proc.register_class r = cl then
                                  score.(n) <- score.(n) - w
              | _ -> ())
            neighbour.prefer)
        reg.interf;
      (* Pick the location with the best score *)
      let best_score = ref (-1000000) and best_slot = ref (-1) in
      for n = 0 to nslots - 1 do
        if score.(n) > !best_score then begin
          best_score := score.(n);
          best_slot := n
        end
      done;
      (* Found one? *)
      if !best_slot >= 0 then
        reg.loc <- Stack(Local !best_slot)
      else begin
        (* Allocate a new stack slot *)
        reg.loc <- Stack(Local nslots);
        Proc.num_stack_slots.(cl) <- nslots + 1
      end
    end
  end;
  (* Cancel the preferences of this register so that they don't influence
     transitively the allocation of registers that prefer this reg. *)
  reg.prefer <- []
(*e: function [[Coloring.assign_location]] *)

(*s: function [[Coloring.allocate_registers]] *)
let allocate_registers() =
  (* First pass: preallocate spill registers
     Second pass: compute the degrees
     Third pass: determine coloring order by successive removals of regs
     Fourth pass: assign registers in that order *)
  for i = 0 to Proc.num_register_classes - 1 do
    Proc.num_stack_slots.(i) <- 0;
    start_register.(i) <- 0
  done;
  List.iter allocate_spilled (Reg.all_registers());
  List.iter find_degree (Reg.all_registers());
  List.iter assign_location (remove_all_regs [])
(*e: function [[Coloring.allocate_registers]] *)
(*e: asmcomp/coloring.ml *)
