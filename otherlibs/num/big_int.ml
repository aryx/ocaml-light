(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


open Int_misc
open Nat

type big_int = 
   { sign : int; 
     abs_value : nat }

let create_big_int sign nat =  
 if sign = 1 or sign = -1 or
    (sign = 0 &
     is_zero_nat nat 0 (num_digits_nat nat 0 (length_nat nat)))
 then { sign = sign; 
         abs_value = nat }
  else invalid_arg "create_big_int"

(* Sign of a big_int *)
let sign_big_int bi = bi.sign

let zero_big_int =
 { sign = 0;
   abs_value = make_nat 1 }

let unit_big_int =  
  { sign = 1;
    abs_value = nat_of_int 1 }

(* Number of digits in a big_int *)
let num_digits_big_int bi = 
 num_digits_nat (bi.abs_value) 0 (length_nat bi.abs_value) 

(* Opposite of a big_int *)
let minus_big_int bi = 
 { sign = - bi.sign;
   abs_value = copy_nat (bi.abs_value) 0 (num_digits_big_int bi)}

(* Absolute value of a big_int *)
let abs_big_int bi = 
    { sign = if bi.sign = 0 then 0 else 1;
      abs_value = copy_nat (bi.abs_value) 0 (num_digits_big_int bi)}

(* Comparison operators on big_int *)

(* 
   compare_big_int (bi, bi2) = sign of (bi-bi2) 
   i.e. 1 if bi > bi2
        0 if bi = bi2
        -1 if bi < bi2
*)
let compare_big_int bi1 bi2 =
  if bi1.sign = 0 & bi2.sign = 0 then 0
  else if bi1.sign < bi2.sign then -1
  else if bi1.sign > bi2.sign then 1
  else if bi1.sign = 1 then
            compare_nat (bi1.abs_value) 0 (num_digits_big_int bi1) 
                        (bi2.abs_value) 0 (num_digits_big_int bi2)
  else
            compare_nat (bi2.abs_value) 0 (num_digits_big_int bi2) 
                        (bi1.abs_value) 0 (num_digits_big_int bi1)

let eq_big_int bi1 bi2 = compare_big_int bi1 bi2 = 0
and le_big_int bi1 bi2 = compare_big_int bi1 bi2 <= 0
and ge_big_int bi1 bi2 = compare_big_int bi1 bi2 >= 0
and lt_big_int bi1 bi2 = compare_big_int bi1 bi2 < 0
and gt_big_int bi1 bi2 = compare_big_int bi1 bi2 > 0

let max_big_int bi1 bi2 = if lt_big_int bi1 bi2 then bi2 else bi1
and min_big_int bi1 bi2 = if gt_big_int bi1 bi2 then bi2 else bi1

(* Operations on big_int *)

let pred_big_int bi = 
 match bi.sign with
    0 -> { sign = -1; abs_value = nat_of_int 1}
  | 1 -> let size_bi = num_digits_big_int bi in
          let copy_bi = copy_nat (bi.abs_value) 0 size_bi in
            decr_nat copy_bi 0 size_bi 0;
            { sign = if is_zero_nat copy_bi 0 size_bi then 0 else 1;
              abs_value = copy_bi }
  | _ -> let size_bi = num_digits_big_int bi in
         let size_res = succ (size_bi) in
         let copy_bi = create_nat (size_res) in
          blit_nat copy_bi 0 (bi.abs_value) 0 size_bi;
          set_digit_nat copy_bi size_bi 0;
          incr_nat copy_bi 0 size_res 1; 
          { sign = -1;
            abs_value = copy_bi }

let succ_big_int bi =
 match bi.sign with
    0 -> {sign = 1; abs_value = nat_of_int 1}
  | -1 -> let size_bi = num_digits_big_int bi in
           let copy_bi = copy_nat (bi.abs_value) 0 size_bi in
            decr_nat copy_bi 0 size_bi 0;
            { sign = if is_zero_nat copy_bi 0 size_bi then 0 else -1;
              abs_value = copy_bi }
  | _ -> let size_bi = num_digits_big_int bi in
         let size_res = succ (size_bi) in
         let copy_bi = create_nat (size_res) in
          blit_nat copy_bi 0 (bi.abs_value) 0 size_bi;
          set_digit_nat copy_bi size_bi 0;
          incr_nat copy_bi 0 size_res 1;
          { sign = 1;
            abs_value = copy_bi }

let add_big_int bi1 bi2 = 
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
  if bi1.sign = bi2.sign
   then    (* Add absolute values if signs are the same *)
    { sign = bi1.sign;
      abs_value = 
       match compare_nat (bi1.abs_value) 0 size_bi1 
                         (bi2.abs_value) 0 size_bi2 with
        -1 -> let res = create_nat (succ size_bi2) in
                (blit_nat res 0 (bi2.abs_value) 0 size_bi2; 
                 set_digit_nat res size_bi2 0;
                 add_nat res 0 (succ size_bi2) 
                          (bi1.abs_value) 0 size_bi1 0;
                 res)
       |_  -> let res = create_nat (succ size_bi1) in
               (blit_nat res 0 (bi1.abs_value) 0 size_bi1;
                set_digit_nat res size_bi1 0;
                add_nat res 0 (succ size_bi1) 
                         (bi2.abs_value) 0 size_bi2 0;
                res)}

  else      (* Subtract absolute values if signs are different *)
    match compare_nat (bi1.abs_value) 0 size_bi1 
                      (bi2.abs_value) 0 size_bi2 with
       0 -> zero_big_int
     | 1 -> { sign = bi1.sign;
               abs_value = 
                let res = copy_nat (bi1.abs_value) 0 size_bi1 in
                 (sub_nat res 0 size_bi1 
                           (bi2.abs_value) 0 size_bi2 1;
                  res) }
     | _ -> { sign = bi2.sign;
              abs_value = 
               let res = copy_nat (bi2.abs_value) 0 size_bi2 in
                 (sub_nat res 0 size_bi2 
                           (bi1.abs_value) 0 size_bi1 1;
                  res) }

(* Coercion with int type *)
let big_int_of_int i =
  { sign = sign_int i;
    abs_value =
      let res = (create_nat 1)
      in (if i = monster_int
             then (set_digit_nat res 0 biggest_int;
                   incr_nat res 0 1 1; ())
             else set_digit_nat res 0 (abs i));
      res }

let add_int_big_int i bi = add_big_int (big_int_of_int i) bi

let sub_big_int bi1 bi2 = add_big_int bi1 (minus_big_int bi2)

(* Returns i * bi *)
let mult_int_big_int i bi =
 let size_bi = num_digits_big_int bi in
 let size_res = succ size_bi in
  if i = monster_int
     then let res = create_nat size_res in
            blit_nat res 0 (bi.abs_value) 0 size_bi;
            mult_digit_nat res 0 size_res (bi.abs_value) 0 size_bi 
                           (nat_of_int biggest_int) 0;
            { sign = - (sign_big_int bi);
              abs_value = res }             
     else let res = make_nat (size_res) in
          mult_digit_nat res 0 size_res (bi.abs_value) 0 size_bi 
                         (nat_of_int (abs i)) 0;
          { sign = (sign_int i) * (sign_big_int bi);
            abs_value = res } 

let mult_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
 let size_res = size_bi1 + size_bi2 in
 let res = make_nat (size_res) in
  { sign = bi1.sign * bi2.sign;
    abs_value = 
         if size_bi2 > size_bi1
           then (mult_nat res 0 size_res (bi2.abs_value) 0 size_bi2 
                          (bi1.abs_value) 0 size_bi1;res)
           else (mult_nat res 0 size_res (bi1.abs_value) 0 size_bi1 
                          (bi2.abs_value) 0 size_bi2;res) }

(* (quotient, rest) of the euclidian division of 2 big_int *)
let quomod_big_int bi1 bi2 =
 if bi2.sign = 0 then raise Division_by_zero
 else
  let size_bi1 = num_digits_big_int bi1
  and size_bi2 = num_digits_big_int bi2 in
   match compare_nat (bi1.abs_value) 0 size_bi1 
                     (bi2.abs_value) 0 size_bi2 with
      -1 -> (* 1/2 -> 0, reste 1, -1/2 -> -1, reste 1 *)
             if bi1.sign = -1
              then (big_int_of_int(-1), add_big_int bi2 bi1)
              else (big_int_of_int 0, bi1)
    | 0 -> (big_int_of_int (bi1.sign * bi2.sign), zero_big_int)
    | _ -> let bi1_negatif = bi1.sign = -1 in 
           let size_q =
            if bi1_negatif 
             then succ (max (succ (size_bi1 - size_bi2)) 1)
             else max (succ (size_bi1 - size_bi2)) 1
           and size_r = succ (max size_bi1 size_bi2) 
            (* r is long enough to contain both quotient and remainder *)
            (* of the euclidian division *)
           in
            (* set up quotient, remainder *)
            let q = create_nat size_q
            and r = create_nat size_r in
            blit_nat r 0 (bi1.abs_value) 0 size_bi1;
            set_to_zero_nat r size_bi1 (size_r - size_bi1);

            (* do the division of |bi1| by |bi2|
               - at the beginning, r contains |bi1| 
               - at the end, r contains 
                 * in the size_bi2 least significant digits, the remainder 
                 * in the size_r-size_bi2 most significant digits, the quotient
              note the conditions for application of div_nat are verified here 
             *)
            div_nat r 0 size_r (bi2.abs_value) 0 size_bi2;

            (* separate quotient and remainder *)
            blit_nat q 0 r size_bi2 (size_r - size_bi2);
            let not_null_mod = not (is_zero_nat r 0 size_bi2) in

            (* correct the signs, adjusting the quotient and remainder *)
            if bi1_negatif & not_null_mod
             then 
              (* bi1<0, r>0, noting r for (r, size_bi2) the remainder,      *)
              (* we have |bi1|=q * |bi2| + r with 0 < r < |bi2|,            *)
              (* thus -bi1 = q * |bi2| + r                                  *)
              (* and bi1 = (-q) * |bi2| + (-r) with -|bi2| < (-r) < 0       *)
              (* thus bi1 = -(q+1) * |bi2| + (|bi2|-r)                      *)
              (* with 0 < (|bi2|-r) < |bi2|                                 *)
              (* so the quotient has for sign the opposite of the bi2'one   *)
              (*                 and for value q+1                          *)
              (* and the remainder is strictly positive                     *)
              (*                  has for value |bi2|-r                     *)
              (let new_r = copy_nat (bi2.abs_value) 0 size_bi2 in
                      (* new_r contains (r, size_bi2) the remainder *)
                { sign = - bi2.sign;
                  abs_value = (set_digit_nat q (pred size_q) 0;
                               incr_nat q 0 size_q 1; q) }, 
                { sign = 1;
                 abs_value = 
                      (sub_nat new_r 0 size_bi2 r 0 size_bi2 1; 
                      new_r) })
             else 
              (if bi1_negatif then set_digit_nat q (pred size_q) 0; 
                { sign = if is_zero_nat q 0 size_q 
                          then 0 
                          else bi1.sign * bi2.sign;
                  abs_value = q },
                { sign = if not_null_mod then 1 else 0;
                  abs_value = copy_nat r 0 size_bi2 })

let div_big_int bi1 bi2 = fst (quomod_big_int bi1 bi2)
and mod_big_int bi1 bi2 = snd (quomod_big_int bi1 bi2)

let gcd_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1 
 and size_bi2 = num_digits_big_int bi2 in
  if is_zero_nat (bi1.abs_value) 0 size_bi1 then abs_big_int bi2
  else if is_zero_nat (bi2.abs_value) 0 size_bi2 then
        { sign = 1;
          abs_value = bi1.abs_value }
  else
        { sign = 1;
          abs_value = 
           match compare_nat (bi1.abs_value) 0 size_bi1 
                             (bi2.abs_value) 0 size_bi2 with
           0 -> bi1.abs_value
         | 1 ->
            let res = copy_nat (bi1.abs_value) 0 size_bi1 in
            let len = 
              gcd_nat res 0 size_bi1 (bi2.abs_value) 0 size_bi2 in
            copy_nat res 0 len
         | _ ->
            let res = copy_nat (bi2.abs_value) 0 size_bi2 in
            let len = 
              gcd_nat res 0 size_bi2 (bi1.abs_value) 0 size_bi1 in
            copy_nat res 0 len
         }

(* Coercion operators *)

let int_of_big_int bi = 
  try bi.sign * int_of_nat bi.abs_value
  with Failure _ ->
    if eq_big_int bi (big_int_of_int monster_int) 
    then monster_int 
    else failwith "int_of_big_int"

let is_int_big_int bi = 
   is_nat_int (bi.abs_value) 0 (num_digits_big_int bi)
or (bi.sign = -1 & num_digits_big_int bi = 1 &
    num_leading_zero_bits_in_digit (bi.abs_value) 0 >= 1)

(* XL: le "1" provient de "pred (length_of_digit - length_of_int))" *)

(* Coercion with nat type *)
let nat_of_big_int bi = 
 if bi.sign = -1
 then failwith "nat_of_big_int"
 else copy_nat (bi.abs_value) 0 (num_digits_big_int bi)

let sys_big_int_of_nat nat off len =
 let length = num_digits_nat nat off len in 
    { sign = if is_zero_nat nat off  length then 0 else 1;
      abs_value = copy_nat nat off length }

let big_int_of_nat nat =
 sys_big_int_of_nat nat 0 (length_nat nat)

(* Coercion with string type *)

let string_of_big_int bi =
  if bi.sign = -1
  then "-" ^ string_of_nat bi.abs_value
  else string_of_nat bi.abs_value

(* XL: j'ai puissamment simplifie "big_int_of_string", en virant
   la notation scientifique (123e6 ou 123.456e12). *)

let sys_big_int_of_string s ofs len =
  let (sign, nat) =
    match s.[ofs] with
      '-' -> if len > 1
                then (-1, sys_nat_of_string 10 s (ofs+1) (len-1))
                else failwith "sys_big_int_of_string"
    | '+' -> if len > 1
                then (1, sys_nat_of_string 10 s (ofs+1) (len-1))
                else failwith "sys_big_int_of_string"
    | _ -> if len > 0
              then (1, sys_nat_of_string 10 s ofs len)
              else failwith "sys_big_int_of_string" in
  { sign = if is_zero_nat nat 0 (length_nat nat) then 0 else sign;
    abs_value = nat }

let big_int_of_string s =
  sys_big_int_of_string s 0 (String.length s)

let power_base_nat base nat off len =
  if is_zero_nat nat off len then nat_of_int 1 else
  let power_base = make_nat (succ length_of_digit) in
  let (pmax, pint) = make_power_base base power_base in
  let (n, rem) = 
      let (x, y) = quomod_big_int (sys_big_int_of_nat nat off len) 
                                  (big_int_of_int (succ pmax)) in
        (int_of_big_int x, int_of_big_int y) in       
  if n = 0 then copy_nat power_base (pred rem) 1 else
   begin
    let res = make_nat n
    and res2 = make_nat n
    and l = num_bits_int n - 2 in
    let p = ref (1 lsl l) in
      blit_nat res 0 power_base pmax 1;
      for i = l downto 0 do
        let len = num_digits_nat res 0 n in
        let len2 = min n (2 * len) in
        let succ_len2 = succ len2 in
          square_nat res2 0 len2 res 0 len;
          begin
           if n land !p > 0
              then (set_to_zero_nat res 0 len;
                    mult_digit_nat res 0 succ_len2 
                                   res2 0 len2 
                                   power_base pmax; ())
              else blit_nat res 0 res2 0 len2
          end;
          set_to_zero_nat res2 0 len2;
          p := !p lsr 1
      done;
    if rem > 0
     then (mult_digit_nat res2 0 n 
                          res 0 n power_base (pred rem);
           res2)
     else res
  end

let power_int_positive_int i n = 
  match sign_int n with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_int"
  | _ -> let nat = power_base_int (abs i) n in
           { sign = if i >= 0
                       then sign_int i 
                       else if n land 1 = 0
                               then 1 
                               else -1;
             abs_value = nat} 

let power_big_int_positive_int bi n = 
  match sign_int n with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_big_int_positive_int"
  | _ -> let bi_len = num_digits_big_int bi in
         let res_len = bi_len * n in
         let res = make_nat res_len 
         and res2 = make_nat res_len 
         and l = num_bits_int n - 2 in
         let p = ref (1 lsl l) in
           blit_nat res 0 (bi.abs_value) 0 bi_len;
           for i = l downto 0 do
             let len = num_digits_nat res 0 res_len in
             let len2 = min res_len (2 * len) in
             let succ_len2 = succ len2 in
               square_nat res2 0 len2 res 0 len;
               (if n land !p > 0 
                   then (set_to_zero_nat res 0 len;
                         mult_nat res 0 succ_len2 
                                   res2 0 len2 (bi.abs_value) 0 bi_len;
                         set_to_zero_nat res2 0 len2)
                   else blit_nat res 0 res2 0 len2;
                   set_to_zero_nat res2 0 len2);
               p := !p lsr 1
           done;
           {sign = if bi.sign >=  0
                      then bi.sign 
                      else if n land 1 = 0
                              then 1 
                              else -1;
            abs_value = res} 

let power_int_positive_big_int i bi = 
  match sign_big_int bi with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_big_int"
  | _ -> let nat = power_base_nat 
                     (abs i) (bi.abs_value) 0 (num_digits_big_int bi) in
           { sign = if i >= 0
                       then sign_int i 
                       else if is_digit_odd (bi.abs_value) 0
                               then -1 
                               else 1;
             abs_value = nat } 

let power_big_int_positive_big_int bi1 bi2 = 
  match sign_big_int bi2 with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_big_int_positive_big_int"
  | _ -> let nat = bi2.abs_value
         and off = 0 
         and len_bi2 = num_digits_big_int bi2 in
         let bi1_len = num_digits_big_int bi1 in
         let res_len = int_of_big_int (mult_int_big_int bi1_len bi2) in
         let res = make_nat res_len 
         and res2 = make_nat res_len 
         and l = (len_bi2 * length_of_digit 
                  - num_leading_zero_bits_in_digit nat (pred len_bi2)) - 2 in
         let p = ref (1 lsl l) in
           blit_nat res 0 (bi1.abs_value) 0 bi1_len;
           for i = l downto 0 do
             let nat = bi2.abs_value in
             let len = num_digits_nat res 0 res_len in
             let len2 = min res_len (2 * len) in
             let succ_len2 = succ len2 in
               square_nat res2 0 len2 res 0 len;
               land_digit_nat nat 0 (nat_of_int !p) 0;
               if is_zero_nat nat 0 len_bi2 
                  then (blit_nat res 0 res2 0 len2;
                        set_to_zero_nat res2 0 len2)
                  else (set_to_zero_nat res 0 len;
                        mult_nat res 0 succ_len2 
                                 res2 0 len2 (bi1.abs_value) 0 bi1_len;
                        set_to_zero_nat res2 0 len2);
               p := !p lsr 1
           done;
           {sign = if bi1.sign >= 0
                      then bi1.sign 
                      else if is_digit_odd (bi2.abs_value) 0
                              then -1 
                              else 1;
            abs_value = res} 

(* base_power_big_int compute bi*base^n *)
let base_power_big_int base n bi =
  match sign_int n with
    0 -> bi
  | -1 -> let nat = power_base_int base (-n) in
           let len_nat = num_digits_nat nat 0 (length_nat nat) 
           and len_bi = num_digits_big_int bi in
             if len_bi < len_nat then
               invalid_arg "base_power_big_int"
             else if len_bi = len_nat &
                     compare_digits_nat (bi.abs_value) len_bi nat len_nat = -1
               then invalid_arg "base_power_big_int"
             else
               let copy = create_nat (succ len_bi) in
                      blit_nat copy 0 (bi.abs_value) 0 len_bi;
                      set_digit_nat copy len_bi 0;
                      div_nat copy 0 (succ len_bi) 
                              nat 0 len_nat;
                      if not (is_zero_nat copy 0 len_nat) 
                         then invalid_arg "base_power_big_int"
                         else { sign = bi.sign;
                                abs_value = copy_nat copy len_nat 1 }
  | _ -> let nat = power_base_int base n in
         let len_nat = num_digits_nat nat 0 (length_nat nat) 
         and len_bi = num_digits_big_int bi in
         let new_len = len_bi + len_nat in
         let res = make_nat new_len in
           (if len_bi > len_nat
               then mult_nat res 0 new_len 
                              (bi.abs_value) 0 len_bi 
                              nat 0 len_nat
               else mult_nat res 0 new_len 
                              nat 0 len_nat 
                              (bi.abs_value) 0 len_bi)
          ; if is_zero_nat res 0 new_len
               then zero_big_int
               else create_big_int (bi.sign) res

(* Coercion with float type *)

let float_of_big_int bi = 
  float_of_string (string_of_big_int bi)

(* XL: suppression de big_int_of_float et nat_of_float. *)

(* Other functions needed *)

(* Integer part of the square root of a big_int *)
let sqrt_big_int bi =
 match bi.sign with 
   -1 -> invalid_arg "sqrt_big_int"
 | 0  ->  {sign = 0;
           abs_value = make_nat (1)}
 |  _  -> {sign = 1;
           abs_value = sqrt_nat (bi.abs_value) 0 (num_digits_big_int bi)}

let square_big_int bi =
  let len_bi = num_digits_big_int bi in
  let len_res = 2 * len_bi in
  let res = make_nat len_res in
    square_nat res 0 len_res (bi.abs_value) 0 len_bi;
    { sign = bi.sign;
      abs_value = res }

(* round off of the futur last digit (of the integer represented by the string
   argument of the function) that is now the previous one.
   if s contains an integer of the form (10^n)-1 
    then s <- only 0 digits and the result_int is true
   else s <- the round number and the result_int is false *)
let round_futur_last_digit s off_set length =
 let l = pred (length + off_set) in 
  if Char.code(String.get s l) >= Char.code '5'
    then
     let rec round_rec l = 
      let current_char = String.get s l in 
       if current_char = '9'
        then
         (String.set s l '0';
          if l = off_set then true else round_rec (pred l))
        else 
         (String.set s l (Char.chr (succ (Char.code current_char)));
          false)
     in round_rec (pred l)
   else false
 

(* Approximation with floating decimal point a` la approx_ratio_exp *)
let approx_big_int prec bi =
  let len_bi = num_digits_big_int bi in
  let n = 
    max 0
        (int_of_big_int (
          add_int_big_int 
            (-prec) 
            (div_big_int (mult_big_int (big_int_of_int (pred len_bi)) 
                                      (big_int_of_string "963295986")) 
                        (big_int_of_string "100000000")))) in
  let s =
    string_of_big_int (div_big_int bi (power_int_positive_int 10 n)) in
  let (sign, off, len) = 
    if String.get s 0 = '-'
       then ("-", 1, succ prec)
       else ("", 0, prec) in
  if (round_futur_last_digit s off (succ prec))
       then (sign^"1."^(String.make prec '0')^"e"^
             (string_of_int (n + 1 - off + String.length s)))
       else (sign^(String.sub s off 1)^"."^
             (String.sub s (succ off) (pred prec))
             ^"e"^(string_of_int (n - succ off + String.length s)))
