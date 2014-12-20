
open BatFullNumber

include (BatInt : module type of BatInt
  with module Infix := BatInt.Infix
  and module Safe_int := BatInt.Safe_int)

let enum () =
  let current_value   = ref min_int in
  let already_through = ref false   in
  let f  () =
    if  !current_value = max_int then
      if !already_through then raise BatEnum.No_more_elements
      else ( already_through := true; max_int )
    else BatRef.post_incr current_value
  in BatEnum.from f


let print out t = BatInnerIO.nwrite out (string_of_int t)
let print_hex out t = BatPrintf.fprintf out "%X" t

let ( -- )  x y = BatEnum.seq x (add one) ((>=) y)
let ( --- ) x y =
  if x <= y then x -- y
  else BatEnum.seq x pred ((<=) y)

