(* Test for output_value / input_value *)

type t = A | B of int | C of float | D of string | E of char
       | F of t | G of t * t | H of int * t | I of t * float | J

let longstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
let verylongstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let test_out filename =
  let oc = open_out filename in
  Marshal.to_channel oc (1) [];
  Marshal.to_channel oc ((-1)) [];
  Marshal.to_channel oc (258) [];
  Marshal.to_channel oc (20000) [];
  Marshal.to_channel oc (0x12345678) [];
  Marshal.to_channel oc (0x123456789ABCDEF0) [];
  Marshal.to_channel oc ("foobargeebuz") [];
  Marshal.to_channel oc (longstring) [];
  Marshal.to_channel oc (verylongstring) [];
  Marshal.to_channel oc (3.141592654) [];
  Marshal.to_channel oc (()) [];
  Marshal.to_channel oc (A) [];
  Marshal.to_channel oc ((B 1)) [];
  Marshal.to_channel oc ((C 2.718)) [];
  Marshal.to_channel oc ((D "hello, world!")) [];
  Marshal.to_channel oc ((E 'l')) [];
  Marshal.to_channel oc ((F(B 1))) [];
  Marshal.to_channel oc ((G(A, G(B 2, G(C 3.14, G(D "glop", E 'e')))))) [];
  Marshal.to_channel oc ((H(1, A))) [];
  Marshal.to_channel oc ((I(B 2, 1e-6))) [];
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  Marshal.to_channel oc (z) [];
  Marshal.to_channel oc ([|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]) [];
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  Marshal.to_channel oc ((big 1000)) [];
  Marshal.to_channel oc y [Marshal.No_sharing];
  Marshal.to_channel oc fib [Marshal.Closures];
  close_out oc


let test n b =
  prerr_string "Test "; prerr_int n;
  if b then prerr_string " passed.\n" else prerr_string " FAILED.\n";
  flush stderr

let test_in filename =
  let ic = open_in filename in
  test 1 (Marshal.from_channel ic = 1);
  test 2 (Marshal.from_channel ic = (-1));
  test 3 (Marshal.from_channel ic = 258);
  test 4 (Marshal.from_channel ic = 20000);
  test 5 (Marshal.from_channel ic = 0x12345678);
  test 6 (Marshal.from_channel ic = 0x123456789ABCDEF0);
  test 7 (Marshal.from_channel ic = "foobargeebuz");
  test 8 (Marshal.from_channel ic = longstring);
  test 9 (Marshal.from_channel ic = verylongstring);
  test 10 (Marshal.from_channel ic = 3.141592654);
  test 11 (Marshal.from_channel ic = ());
  test 12 (match Marshal.from_channel ic with
    A -> true
  | _ -> false);
  test 13 (match Marshal.from_channel ic with
    (B 1) -> true
  | _ -> false);
  test 14 (match Marshal.from_channel ic with
    (C f) -> f = 2.718
  | _ -> false);
  test 15 (match Marshal.from_channel ic with
    (D "hello, world!") -> true
  | _ -> false);
  test 16 (match Marshal.from_channel ic with
    (E 'l') -> true
  | _ -> false);
  test 17 (match Marshal.from_channel ic with
    (F(B 1)) -> true
  | _ -> false);
  test 18 (match Marshal.from_channel ic with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  test 19 (match Marshal.from_channel ic with
    (H(1, A)) -> true
  | _ -> false);
  test 20 (match Marshal.from_channel ic with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  test 21 (match Marshal.from_channel ic with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 & t3 == t5 & t4 == t1
  | _ -> false);
  test 22 (Marshal.from_channel ic = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec check_big n t =
    if n <= 0 then
      test 23 (match t with A -> true | _ -> false)
    else
      match t with H(m, s) -> if m = n then check_big (n-1) s
                                       else test 23 false
                 | _ -> test 23 false
  in
    check_big 1000 (Marshal.from_channel ic);
  test 24 (match Marshal.from_channel ic with
    G((D "sharing" as t1), (D "sharing" as t2)) -> t1 != t2
  | _ -> false);
  test 25 (let fib = Marshal.from_channel ic in fib 5 = 8 && fib 10 = 89);
  close_in ic

let test_string () =
  let s = Marshal.to_string 1 [] in
  test 101 (Marshal.from_string s 0 = 1);
  let s = Marshal.to_string (-1) [] in
  test 102 (Marshal.from_string s 0 = (-1));
  let s = Marshal.to_string 258 [] in
  test 103 (Marshal.from_string s 0 = 258);
  let s = Marshal.to_string 20000 [] in
  test 104 (Marshal.from_string s 0 = 20000);
  let s = Marshal.to_string 0x12345678 [] in
  test 105 (Marshal.from_string s 0 = 0x12345678);
  let s = Marshal.to_string 0x123456789ABCDEF0 [] in
  test 106 (Marshal.from_string s 0 = 0x123456789ABCDEF0);
  let s = Marshal.to_string "foobargeebuz" [] in
  test 107 (Marshal.from_string s 0 = "foobargeebuz");
  let s = Marshal.to_string longstring [] in
  test 108 (Marshal.from_string s 0 = longstring);
  let s = Marshal.to_string verylongstring [] in
  test 109 (Marshal.from_string s 0 = verylongstring);
  let s = Marshal.to_string 3.141592654 [] in
  test 110 (Marshal.from_string s 0 = 3.141592654);
  let s = Marshal.to_string () [] in
  test 111 (Marshal.from_string s 0 = ());
  let s = Marshal.to_string A [] in
  test 112 (match Marshal.from_string s 0 with
    A -> true
  | _ -> false);
  let s = Marshal.to_string (B 1) [] in
  test 113 (match Marshal.from_string s 0 with
    (B 1) -> true
  | _ -> false);
  let s = Marshal.to_string (C 2.718) [] in
  test 114 (match Marshal.from_string s 0 with
    (C f) -> f = 2.718
  | _ -> false);
  let s = Marshal.to_string (D "hello, world!") [] in
  test 115 (match Marshal.from_string s 0 with
    (D "hello, world!") -> true
  | _ -> false);
  let s = Marshal.to_string (E 'l') [] in
  test 116 (match Marshal.from_string s 0 with
    (E 'l') -> true
  | _ -> false);
  let s = Marshal.to_string (F(B 1)) [] in
  test 117 (match Marshal.from_string s 0 with
    (F(B 1)) -> true
  | _ -> false);
  let s = Marshal.to_string (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) [] in
  test 118 (match Marshal.from_string s 0 with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  let s = Marshal.to_string (H(1, A)) [] in
  test 119 (match Marshal.from_string s 0 with
    (H(1, A)) -> true
  | _ -> false);
  let s = Marshal.to_string (I(B 2, 1e-6)) [] in
  test 120 (match Marshal.from_string s 0 with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  let s = Marshal.to_string z [] in
  test 121 (match Marshal.from_string s 0 with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 & t3 == t5 & t4 == t1
  | _ -> false);
  let s = Marshal.to_string [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|] [] in
  test 122 (Marshal.from_string s 0 = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  let s = Marshal.to_string (big 1000) [] in
  let rec check_big n t =
    if n <= 0 then
      test 123 (match t with A -> true | _ -> false)
    else
      match t with H(m, s) -> if m = n then check_big (n-1) s
                                       else test 123 false
                 | _ -> test 123 false
  in
    check_big 1000 (Marshal.from_string s 0)

let test_buffer () =
  let s = Bytes.create 512 in
  Marshal.to_buffer s 0 512 1 [];
  test 201 (Marshal.from_bytes s 0 = 1);
  Marshal.to_buffer s 0 512 (-1) [];
  test 202 (Marshal.from_bytes s 0 = (-1));
  Marshal.to_buffer s 0 512 258 [];
  test 203 (Marshal.from_bytes s 0 = 258);
  Marshal.to_buffer s 0 512 20000 [];
  test 204 (Marshal.from_bytes s 0 = 20000);
  Marshal.to_buffer s 0 512 0x12345678 [];
  test 205 (Marshal.from_bytes s 0 = 0x12345678);
  Marshal.to_buffer s 0 512 0x123456789ABCDEF0 [];
  test 206 (Marshal.from_bytes s 0 = 0x123456789ABCDEF0);
  Marshal.to_buffer s 0 512 "foobargeebuz" [];
  test 207 (Marshal.from_bytes s 0 = "foobargeebuz");
  Marshal.to_buffer s 0 512 longstring [];
  test 208 (Marshal.from_bytes s 0 = longstring);
  test 209
    (try Marshal.to_buffer s 0 512 verylongstring []; false
     with Failure "Marshal.to_buffer: buffer overflow" -> true);
  Marshal.to_buffer s 0 512 3.141592654 [];
  test 210 (Marshal.from_bytes s 0 = 3.141592654);
  Marshal.to_buffer s 0 512 () [];
  test 211 (Marshal.from_bytes s 0 = ());
  Marshal.to_buffer s 0 512 A [];
  test 212 (match Marshal.from_bytes s 0 with
    A -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (B 1) [];
  test 213 (match Marshal.from_bytes s 0 with
    (B 1) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (C 2.718) [];
  test 214 (match Marshal.from_bytes s 0 with
    (C f) -> f = 2.718
  | _ -> false);
  Marshal.to_buffer s 0 512 (D "hello, world!") [];
  test 215 (match Marshal.from_bytes s 0 with
    (D "hello, world!") -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (E 'l') [];
  test 216 (match Marshal.from_bytes s 0 with
    (E 'l') -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (F(B 1)) [];
  test 217 (match Marshal.from_bytes s 0 with
    (F(B 1)) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) [];
  test 218 (match Marshal.from_bytes s 0 with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (H(1, A)) [];
  test 219 (match Marshal.from_bytes s 0 with
    (H(1, A)) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (I(B 2, 1e-6)) [];
  test 220 (match Marshal.from_bytes s 0 with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  Marshal.to_buffer s 0 512 z [];
  test 221 (match Marshal.from_bytes s 0 with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 & t3 == t5 & t4 == t1
  | _ -> false);
  Marshal.to_buffer s 0 512 [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|] [];
  test 222 (Marshal.from_bytes s 0 = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  test 223
    (try Marshal.to_buffer s 0 512 (big 1000) []; false
     with Failure "Marshal.to_buffer: buffer overflow" -> true)

let test_size() =
  let s = Marshal.to_string (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) [] in
  test 300 (Marshal.header_size + Marshal.data_size (Bytes.of_string s) 0 = String.length s)

let main() =
  test_out "intext.data"; test_in "intext.data";
  test_out "intext.data"; test_in "intext.data";
  Sys.remove "intext.data";
  test_string();
  test_buffer();
  test_size()

let _ = Printexc.catch main (); exit 0
