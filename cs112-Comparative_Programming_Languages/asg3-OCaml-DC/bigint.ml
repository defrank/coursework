(* $Id: bigint.ml,v 1.4 2011-05-10 08:45:09-07 dmfrank - $ *)
(* Derek Frank, dmfrank@ucsc.edu *)

open Printf
exception Empty_list

module Bigint = struct

  type sign     = Pos | Neg
  type bigint   = Bigint of sign * int list
  let  radix    = 1000
  let  radixlen =    3

  let length    = List.length
  let car       = List.hd
  let cdr       = List.tl
  let map       = List.map
  let reverse   = List.rev
  let strcat    = String.concat
  let strlen    = String.length
  let strsub    = String.sub
  let zero      = Bigint (Pos, [])

  let bigint_of_string str =
    let rec makelist str first len =
      if   first = len  then
        [] 
      else let diff = len - first in
           let len' = len - radixlen in
           if   diff < radixlen then
             [int_of_string (strsub str first diff)]
           else (int_of_string (strsub str len' radixlen))
             :: (makelist str first len')
    in  let len = strlen str
        in  if   len = 0  then
            Bigint (Pos, [])
          else if   str.[0] = '_'  then
            Bigint (Neg, makelist str 1 len)
          else Bigint (Pos, makelist str 0 len)
 
  let string_of_bigint (Bigint (sign, value)) =
    match value with
      | []    -> "0"
      | value -> let reversed = reverse value
                 in  strcat ""
                 ((if sign = Pos then "" else "-") ::
                     (string_of_int (car reversed)) ::
                     (map (sprintf "%03d") (cdr reversed)))

  let rec get_last list = match list with
      [] -> raise Empty_list
    | h::[] -> h
    | h::t -> get_last t

  let rec lead_zeros' rev = 
    if (car rev) = 0 then
      lead_zeros' (cdr rev)
    else (reverse rev)

  let lead_zeros list = match list with
    | [0] -> list
    | [] -> []
    | list -> let reversed = reverse list
              in lead_zeros' reversed

  let rec cmp' list1 list2 =
    if list1 = [] then
      0
    else if ((car list1) > (car list2))  then
      1
    else if ((car list1) < (car list2))  then
      -1
    else cmp' (cdr list1) (cdr list2)

  let cmp value1 value2 = 
    if ((value1 = []) or (value2 = []))  then
      raise Empty_list
    else if (length value1) > (length value2)  then
      1
    else if (length value1) < (length value2)  then
      -1
    else cmp' (reverse value1) (reverse value2)

  let rec add' list1 list2 carry = match (list1, list2, carry) with
    | list1, [], 0       -> list1
    | [], list2, 0       -> list2
    | list1, [], carry   -> add' list1 [carry] 0
    | [], list2, carry   -> add' [carry] list2 0
    | car1::cdr1, car2::cdr2, carry -> 
      let sum = car1 + car2 + carry
      in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

  let rec sub' list1 list2 borrow = match (list1, list2, borrow) with 
    | list1, [], 0       -> list1
    | [], list2, 0       -> list2 (** assured this will never occur **)
    | list1, [], borrow  -> sub' list1 [borrow] 0
    | [], list2, borrow  -> [] (** assured this will never occur **)
    | car1::cdr1, car2::cdr2, borrow ->
      let diff = car1 - car2 + borrow
      in if diff < 0 then
          diff + radix :: sub' cdr1 cdr2 (0 - radix)
        else diff :: sub' cdr1 cdr2 0

  let add (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    if sign1 = sign2 then
      Bigint (sign1, add' value1 value2 0)
    else if (cmp value1 value2) = 1 then
      Bigint (sign1, lead_zeros (sub' value1 value2 0))
    else if (cmp value1 value2) = -1 then
      Bigint (sign1, lead_zeros (sub' value2 value1 0))
    else Bigint (Pos, [0])

  let sub (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    if sign1 <> sign2 then
      if sign1 = Pos then
        Bigint (Pos, add' value1 value2 0)
      else Bigint (Neg, add' value1 value2 0)
    else if (cmp value1 value2) = 1 then
      Bigint (sign1, lead_zeros (sub' value1 value2 0))
    else if (cmp value1 value2) = -1 then
      if sign2 = Pos then
        Bigint (Neg, lead_zeros (sub' value2 value1 0))
      else Bigint (Pos, lead_zeros (sub' value2 value1 0))
    else Bigint (Pos, [0])

  let mul = add


(*
  let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    let rec mul' two double diff sum =
      let next_two = add' two two 0
      and next_double = add' double double 0
      in if (cmp next_two diff) >= 1 then
          (sub' diff two, add' sum double)
        else let (z, p) = mul' next_two next_double diff sum

      if (cmp two double) = 1 then
          (sub, sum)
        else if (cmp next_two next_double)) >= 1 then
        
      else mul' (add' two two 0) (add' double double 0) sub sum
  in if sign1 = sign2 then
      Bigint (Pos, mul' [1] value2 value1 0)
    else Bigint (Neg, mul' [1] value2 value1 0)




  let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    let rec mul' two double diff sum = match (two, double, diff, sum)
      with
        | two, double, [0], sum -> sum
        | two, double, diff, sum -> let next_two = add' two two 0
                                   and next_double = add double double 0
                                    in if (cmp next_two diff) = 1 then
                                        sum + double
                                     else if (cmp two diff) = 1 then
                                        if 
                                    in let (zer, product) = mul' [1] *)


  let div = add

  let rem = add

  let pow = add

end

