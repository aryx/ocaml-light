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
open String_misc
open Nat
open Big_int
open Arith_flags

(* Definition of the type ratio :
   Conventions :                                 
   - the denominator is always a positive number 
   - the sign of n/0 is the sign of n            
These convention is automatically respected when a ratio is created with 
the create_ratio primitive
*)

type ratio = { mutable numerator : big_int; 
               mutable denominator : big_int; 
               mutable normalized : bool}

let failwith_zero name = 
    let s = "infinite or undefined rational number" in
    failwith (if String.length name = 0 then s else name ^ " " ^ s)

let numerator_ratio r = r.numerator
and denominator_ratio r = r.denominator

let null_denominator r = sign_big_int r.denominator = 0

let verify_null_denominator r = 
  if sign_big_int r.denominator = 0
     then (if !error_when_null_denominator_flag
           then (failwith_zero "")
           else true)
     else false

let sign_ratio r = sign_big_int r.numerator 

(* Physical normalization of rational numbers *)
(* 1/0, 0/0 and -1/0 are the normalized forms for n/0 numbers *)
let normalize_ratio r = 
  if r.normalized then r
  else if verify_null_denominator r then begin
    r.numerator <- big_int_of_int (sign_big_int r.numerator);
    r.normalized <- true;
    r
  end else begin
    let p = gcd_big_int r.numerator r.denominator in
    if eq_big_int p unit_big_int 
    then begin
      r.normalized <- true; r
    end else begin
      r.numerator <- div_big_int (r.numerator) p; 
      r.denominator <- div_big_int (r.denominator) p; 
      r.normalized <- true; r
    end
  end

let cautious_normalize_ratio r =
 if (!normalize_ratio_flag) then (normalize_ratio r) else r

let cautious_normalize_ratio_when_printing r =
 if (!normalize_ratio_when_printing_flag) then (normalize_ratio r) else r

let create_ratio bi1 bi2 =
 match sign_big_int bi2 with 
       -1 -> cautious_normalize_ratio
               { numerator = minus_big_int bi1; 
                 denominator = minus_big_int bi2;
                 normalized = false }
     | 0 -> if !error_when_null_denominator_flag 
                then (failwith_zero "create_ratio")
                else cautious_normalize_ratio 
                    { numerator = bi1; denominator = bi2; normalized = false }
     | _ ->  cautious_normalize_ratio 
              { numerator = bi1; denominator = bi2; normalized = false }

let create_normalized_ratio bi1 bi2 =
 match sign_big_int bi2 with
  -1 -> { numerator = minus_big_int bi1; 
          denominator = minus_big_int bi2; 
          normalized = true }
|  0 -> if !error_when_null_denominator_flag 
            then failwith_zero "create_normalized_ratio"
            else { numerator = bi1; denominator = bi2; normalized = true }
|  _  -> { numerator = bi1; denominator = bi2; normalized = true }

let is_normalized_ratio r = r.normalized

let report_sign_ratio r bi =
  if sign_ratio r = -1
  then minus_big_int bi 
  else bi

let abs_ratio r = 
 { numerator = abs_big_int r.numerator;
   denominator = r.denominator;
   normalized = r.normalized }

let is_integer_ratio r =
 eq_big_int ((normalize_ratio r).denominator) unit_big_int

(* Operations on rational numbers *)

let add_ratio r1 r2 =
 if !normalize_ratio_flag then begin
    let p = gcd_big_int ((normalize_ratio r1).denominator) 
                        ((normalize_ratio r2).denominator) in
    if eq_big_int p unit_big_int then 
       {numerator = add_big_int (mult_big_int (r1.numerator) r2.denominator) 
                                (mult_big_int (r2.numerator) r1.denominator);
        denominator = mult_big_int (r1.denominator) r2.denominator;
        normalized = true}
    else begin
      let d1 = div_big_int (r1.denominator) p
      and d2 = div_big_int (r2.denominator) p in
      let n = add_big_int (mult_big_int (r1.numerator) d2) 
                          (mult_big_int d1 r2.numerator) in
      let p' = gcd_big_int n p in
        { numerator = div_big_int n p';
          denominator = mult_big_int d1 (div_big_int (r2.denominator) p');
          normalized = true }
      end
 end else
  { numerator = add_big_int (mult_big_int (r1.numerator) r2.denominator) 
                            (mult_big_int (r1.denominator) r2.numerator);
    denominator = mult_big_int (r1.denominator) r2.denominator;
    normalized = false }

let minus_ratio r =
 { numerator = minus_big_int (r.numerator);
   denominator = r.denominator;
   normalized = r.normalized }

let add_int_ratio i r = 
  cautious_normalize_ratio r;
  { numerator = add_big_int (mult_int_big_int i r.denominator) r.numerator;
    denominator = r.denominator;
    normalized = r.normalized }

let add_big_int_ratio bi r = 
  cautious_normalize_ratio r;
  { numerator = add_big_int (mult_big_int bi r.denominator) r.numerator ;
    denominator = r.denominator;
    normalized = r.normalized }

let sub_ratio r1 r2 = add_ratio r1 (minus_ratio r2)

let mult_ratio r1 r2 =
 if !normalize_ratio_flag then begin
   let p1 = gcd_big_int ((normalize_ratio r1).numerator) 
                        ((normalize_ratio r2).denominator) 
   and p2 = gcd_big_int (r2.numerator) r1.denominator in
   let (n1, d2) = 
     if eq_big_int p1 unit_big_int 
         then (r1.numerator, r2.denominator)
         else (div_big_int (r1.numerator) p1, div_big_int (r2.denominator) p1)
   and (n2, d1) =
      if eq_big_int p2 unit_big_int 
         then (r2.numerator, r1.denominator)
         else (div_big_int r2.numerator p2, div_big_int r1.denominator p2) in
    { numerator = mult_big_int n1 n2;
      denominator = mult_big_int d1 d2;
      normalized = true }
 end else
  { numerator = mult_big_int (r1.numerator) r2.numerator;
    denominator = mult_big_int (r1.denominator) r2.denominator;
    normalized = false }

let mult_int_ratio i r = 
 if !normalize_ratio_flag then
  begin
   let p = gcd_big_int ((normalize_ratio r).denominator) (big_int_of_int i) in
   if eq_big_int p unit_big_int 
      then { numerator = mult_big_int (big_int_of_int i) r.numerator;
             denominator = r.denominator;
             normalized = true }
      else { numerator = mult_big_int (div_big_int (big_int_of_int i) p) 
                                      r.numerator;
             denominator = div_big_int (r.denominator) p;
             normalized = true }
  end
 else
  { numerator = mult_int_big_int i r.numerator;
    denominator = r.denominator;
    normalized = false }

let mult_big_int_ratio bi r = 
 if !normalize_ratio_flag then 
  begin
   let p = gcd_big_int ((normalize_ratio r).denominator) bi in
     if eq_big_int p unit_big_int
        then { numerator = mult_big_int bi r.numerator;
               denominator = r.denominator;
               normalized = true }
        else { numerator = mult_big_int (div_big_int bi p) r.numerator;
               denominator = div_big_int (r.denominator) p;
               normalized = true }
  end
 else
  { numerator = mult_big_int bi r.numerator;
      denominator = r.denominator;
      normalized = false }

let square_ratio r =
  cautious_normalize_ratio r;
  { numerator = square_big_int r.numerator;
    denominator = square_big_int r.denominator;
    normalized = r.normalized }

let inverse_ratio r =
  if !error_when_null_denominator_flag & (sign_big_int r.numerator) = 0
  then failwith_zero "inverse_ratio"
  else {numerator = report_sign_ratio r r.denominator; 
        denominator = abs_big_int r.numerator; 
        normalized = r.normalized}

let div_ratio r1 r2 = 
  mult_ratio r1 (inverse_ratio r2)

(* Integer part of a rational number *)
(* Odd function *)
let integer_ratio r = 
 if null_denominator r then failwith_zero "integer_ratio"
 else if sign_ratio r = 0 then zero_big_int
 else report_sign_ratio r (div_big_int (abs_big_int r.numerator) 
                                       (abs_big_int r.denominator))

(* Floor of a rational number *)
(* Always less or equal to r *)
let floor_ratio r = 
 verify_null_denominator r;
 div_big_int (r.numerator) r.denominator

(* Round of a rational number *)
(* Odd function, 1/2 -> 1 *)
let round_ratio r =
 verify_null_denominator r;
  let abs_num = abs_big_int r.numerator in
   let bi = div_big_int abs_num r.denominator in
    report_sign_ratio r 
     (if sign_big_int 
          (sub_big_int 
           (mult_int_big_int 
             2 
             (sub_big_int abs_num (mult_big_int (r.denominator) bi))) 
           r.denominator) = -1
      then bi
      else succ_big_int bi)

let ceiling_ratio r = 
 if (is_integer_ratio r)
 then r.numerator
 else succ_big_int (floor_ratio r)


(* Comparison operators on rational numbers *)
let eq_ratio r1 r2 =
 normalize_ratio r1; 
 normalize_ratio r2;
 eq_big_int (r1.numerator) r2.numerator &
 eq_big_int (r1.denominator) r2.denominator 

let compare_ratio r1 r2 =
  if verify_null_denominator r1 then
        let sign_num_r1 = sign_big_int r1.numerator in
         if (verify_null_denominator r2)
          then 
           let sign_num_r2 = sign_big_int r2.numerator in
             if sign_num_r1 = 1 & sign_num_r2 = -1 then  1 
             else if sign_num_r1 = -1 & sign_num_r2 = 1 then -1
             else 0
         else sign_num_r1
  else if verify_null_denominator r2 then
          -(sign_big_int r2.numerator)
  else match compare_int (sign_big_int r1.numerator) 
                         (sign_big_int r2.numerator) with
               1 -> 1
             | -1 -> -1
             | _ -> if eq_big_int (r1.denominator) r2.denominator 
                    then compare_big_int (r1.numerator) r2.numerator
                    else compare_big_int 
                            (mult_big_int (r1.numerator) r2.denominator) 
                            (mult_big_int (r1.denominator) r2.numerator)
     

let lt_ratio r1 r2 = compare_ratio r1 r2 < 0
and le_ratio r1 r2 = compare_ratio r1 r2 <= 0
and gt_ratio r1 r2 = compare_ratio r1 r2 > 0
and ge_ratio r1 r2 = compare_ratio r1 r2 >= 0

let max_ratio r1 r2 = if lt_ratio r1 r2 then r2 else r1
and min_ratio r1 r2 = if gt_ratio r1 r2 then r2 else r1

let eq_big_int_ratio bi r =
 (is_integer_ratio r) & eq_big_int bi r.numerator

let compare_big_int_ratio bi r =
 normalize_ratio r;
 if (verify_null_denominator r)
 then -(sign_big_int r.numerator)
 else compare_big_int (mult_big_int bi r.denominator) r.numerator

let lt_big_int_ratio bi r = compare_big_int_ratio bi r < 0
and le_big_int_ratio bi r = compare_big_int_ratio bi r <= 0
and gt_big_int_ratio bi r = compare_big_int_ratio bi r > 0
and ge_big_int_ratio bi r = compare_big_int_ratio bi r >= 0

(* Coercions *)

(* Coercions with type int *)
let int_of_ratio r = 
 if ((is_integer_ratio r) & (is_int_big_int r.numerator))
 then (int_of_big_int r.numerator)
 else failwith "integer argument required"

and ratio_of_int i =
 { numerator = big_int_of_int i; 
   denominator = unit_big_int;
   normalized = true }

(* Coercions with type nat *)
let ratio_of_nat nat = 
 { numerator = big_int_of_nat nat;
   denominator = unit_big_int;
   normalized = true }

and nat_of_ratio r =
 normalize_ratio r;
 if not (is_integer_ratio r) then
          failwith "nat_of_ratio"
 else if sign_big_int r.numerator > -1 then
         nat_of_big_int (r.numerator)
 else failwith "nat_of_ratio"

(* Coercions with type big_int *)
let ratio_of_big_int bi = 
 { numerator = bi; denominator = unit_big_int; normalized = true }

and big_int_of_ratio r =
 normalize_ratio r;
 if is_integer_ratio r 
  then r.numerator
 else failwith "big_int_of_ratio"

let div_int_ratio i r = 
  verify_null_denominator r;
  mult_int_ratio i (inverse_ratio r)

let div_ratio_int r i = 
  div_ratio r (ratio_of_int i)

let div_big_int_ratio bi r = 
  verify_null_denominator r;
  mult_big_int_ratio bi (inverse_ratio r)

let div_ratio_big_int r bi = 
  div_ratio r (ratio_of_big_int bi)

(* Functions on type string                                 *)
(* giving floating point approximations of rational numbers *)

let only_zeros s i l =
 let res = ref true in
 for j = i to i + l - 1 do
  if s.[j] <> '0' then res := false
 done;
 !res

(* Position of the leading digit of the decimal expansion          *)
(* of a strictly positive rational number                          *)
(* if the decimal expansion of a non null rational r is equal to   *)
(* sigma for k=-P to N of r_k*10^k then msd_ratio r = N            *)
(* Nota : for a big_int we have msd_ratio = nums_digits_big_int -1 *)
let msd_ratio r =
 cautious_normalize_ratio r;
 if null_denominator r then failwith_zero "msd_ratio"
 else if sign_big_int r.numerator = 0 then 0
 else begin
         let str_num = string_of_big_int r.numerator
         and str_den = string_of_big_int r.denominator in
         let size_num = String.length str_num
         and size_den = String.length str_den in
         let rec msd_rec str_num nnum str_den nden i m = 
           if i > nnum then
            if i > nden or only_zeros str_den i (nden - i)
            then m else pred m
           else if i > nden then m
           else match compare_int (Char.code (String.get str_num i)) 
                                  (Char.code (String.get str_den i)) with
                    0 -> msd_rec str_num nnum str_den nden (succ i) m
                  | 1 -> m
                  |  _ -> pred m
         in msd_rec str_num (pred size_num) str_den (pred size_den) 
                     0 (size_num - size_den)
      end

(* Decimal approximations of rational numbers *)

(* Approximation with fix decimal point *)
(* This is an odd function and the last digit is round off *)
(* Format integer_part . decimal_part_with_n_digits *)
let approx_ratio_fix n r =
 (* Don't need to normalize *)
 if (null_denominator r) then failwith_zero "approx_ratio_fix"
 else
  let sign_r = sign_ratio r in 
   if sign_r = 0
   then "+0" (* r = 0 *)
   else (* r.numerator and r.denominator are not null numbers 
           s contains one more digit than desired for the round off operation
           and to have enough room in s when including the decimal point *)
    if n >= 0 then 
        let s = 
         let nat = 
           (nat_of_big_int
                (div_big_int
                   (base_power_big_int
                       10 (succ n) (abs_big_int r.numerator))
                   r.denominator))
         in (if sign_r = -1 then "-" else "+") ^ string_of_nat nat in
        let l = String.length s in
         if round_futur_last_digit s 1 (pred l) 
          then begin (* if one more char is needed in s *)
           let str = (String.make (succ l) '0') in 
            String.set str 0 (if sign_r = -1 then '-' else '+');
            String.set str 1 '1';
            String.set str (l - n) '.';
            str
          end else (* s can contain the final result *)
           if l > n + 2
            then begin (* |r| >= 1, set decimal point *)
             let l2 = (pred l) - n in
               String.blit s l2 s (succ l2) n; 
               String.set s l2 '.'; s
            end else begin (* |r| < 1, there must be 0-characters *)
                           (* before the significant development, *)
                           (* with care to the sign of the number *)
             let size = n + 3 in
             let m = size - l + 2
             and str = String.make size '0' in

              (String.blit (if sign_r = 1 then "+0." else "-0.") 0 str 0 3);
              (String.blit s 1 str m (l - 2));
              str
            end
       else begin
         let s = string_of_big_int
                   (div_big_int
                      (abs_big_int r.numerator) 
                      (base_power_big_int
                        10 (-n) r.denominator)) in
         let len = succ (String.length s) in
         let s' = String.make len '0' in
          String.set s' 0 (if sign_r = -1 then '-' else '+');
          String.blit s 0 s' 1 (pred len);
          s'
       end

(* Number of digits of the decimal representation of an int *)
let num_decimal_digits_int n = 
  String.length (string_of_int n)

(* Approximation with floating decimal point *)
(* This is an odd function and the last digit is round off *)
(* Format (+/-)(0. n_first_digits e msd)/(1. n_zeros e (msd+1) *)
let approx_ratio_exp n r = 
 (* Don't need to normalize *)
 if (null_denominator r) then failwith_zero "approx_ratio_exp"
 else if n <= 0 then invalid_arg "approx_ratio_exp"
 else
  let sign_r = sign_ratio r
  and i = ref (n + 3) in
   if sign_r = 0
     then
      let s = String.make (n + 5) '0' in
       (String.blit "+0." 0 s 0 3);
       (String.blit "e0" 0 s !i 2); s
   else 
     let msd = msd_ratio (abs_ratio r) in
     let k = n - msd in
     let s = 
      (let nat = nat_of_big_int
                (if k < 0
                  then 
                   div_big_int (abs_big_int r.numerator) 
                               (base_power_big_int 10 (-k) 
                                                   r.denominator)
                 else 
                  div_big_int (base_power_big_int
                                10 k (abs_big_int r.numerator))
                               r.denominator) in
       string_of_nat nat) in
     if (round_futur_last_digit s 0 (String.length s)) 
      then
       let m = num_decimal_digits_int (succ msd) in
       let str = String.make (n + m + 4) '0' in
         (String.blit (if sign_r = -1 then "-1." else "+1.") 0 str 0 3);
         String.set str !i ('e');
         incr i;
         (if m = 0
          then String.set str !i '0' 
          else String.blit (string_of_int (succ msd)) 0 str !i m); 
         str
     else
      let m = num_decimal_digits_int (succ msd)
      and p = n + 3 in
      let str = String.make (succ (m + p)) '0' in
        (String.blit (if sign_r = -1 then "-0." else "+0.") 0 str 0 3);
        (String.blit s 0 str 3 n);
        String.set str p 'e';
        (if m = 0
          then String.set str (succ p) '0'
          else (String.blit (string_of_int (succ msd)) 0 str (succ p) m));
        str

(* String approximation of a rational with a fixed number of significant *)
(* digits printed                                                        *)
let float_of_rational_string r = 
  let s = approx_ratio_exp !floating_precision r in
    if String.get s 0 = '+'
       then (String.sub s 1 (pred (String.length s)))
       else s

(* Coercions with type string *)
let string_of_ratio r = 
 cautious_normalize_ratio_when_printing r;
 if !approx_printing_flag
    then float_of_rational_string r
    else string_of_big_int r.numerator ^ "/" ^ string_of_big_int r.denominator

(* XL: j'ai puissamment simplifie "ratio_of_string" en virant la notation
   scientifique. *)

let ratio_of_string s =
  let n = index_char s '/' 0 in
  if n = -1 then
    { numerator = big_int_of_string s;
      denominator = unit_big_int;
      normalized = true }
  else
    create_ratio (sys_big_int_of_string s 0 n)
                 (sys_big_int_of_string s (n+1) (String.length s - n - 1))

(* Coercion with type float *)

let float_of_ratio r =
  float_of_string (float_of_rational_string r)

(* XL: suppression de ratio_of_float *)

let power_ratio_positive_int r n = 
  create_ratio (power_big_int_positive_int (r.numerator) n) 
               (power_big_int_positive_int (r.denominator) n)

let power_ratio_positive_big_int r bi = 
  create_ratio (power_big_int_positive_big_int (r.numerator) bi) 
               (power_big_int_positive_big_int (r.denominator) bi)
