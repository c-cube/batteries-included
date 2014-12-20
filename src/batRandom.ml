(*
 * BatRandom - Additional randomization operations
 * Copyright (C) 1996 Damien Doligez
 *               2009 David Teller, LIFO, Universite d'Orleans
 *               2009 Pierre Chambart
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


let init      = Random.init
let full_init = Random.full_init
let self_init = Random.self_init
let bits      = Random.bits
let int       = Random.int
let int32     = Random.int32
let int64     = Random.int64
let nativeint = Random.nativeint
let float     = Random.float
let bool      = Random.bool
let char ()   = Char.chr (int 256)

let full_range_int =
  if Sys.word_size = 32 then (* need 31-bits of entropy, bits() gives 30 *)
    fun () -> if bool () then - (bits ())-1 else bits ()
  else (* 64-bit words *)
    fun () -> (* need 63 bits of entropy , bits + bits + bits land 0b11 *)
      let b = (bits ()) lor (bits () lsl 30) lor ((bits () land 0b11) lsl 60) in
      if bool () then b else -b - 1



module State =
struct
  include Random.State

  let char t   = Char.chr (int t 256)

  (**A constructor for generations of random numbers. *)
  let gen_bits state () = fun () -> Some (bits state)
  let gen_int state bound = fun () -> Some (int state bound)
  let gen_int32 state bound = fun () -> Some (int32 state bound)
  let gen_int64 state bound = fun () -> Some (int64 state bound)
  let gen_float state bound = fun () -> Some (float state bound)
  let gen_nativeint state bound =
    fun () -> Some (nativeint state bound)
  let gen_bool state () = fun () -> Some (bool state)
  let gen_char state () = fun () -> Some (char state)

end

let gen_bits () = fun () -> Some (bits ())
let gen_int bound = fun () -> Some (int bound)
let gen_int32 bound = fun () -> Some (int32 bound)
let gen_int64 bound = fun () -> Some (int64 bound)
let gen_float bound = fun () -> Some (float bound)
let gen_nativeint bound = fun () -> Some (nativeint bound)
let gen_bool () = fun () -> Some (bool ())
let gen_char () = fun () -> Some (char ())

let _choice_list l =
  List.nth l (int (List.length l))

let choice_gen g =
  let l = BatGen.to_rev_list g in
  _choice_list l

(* Reservoir sampling algorithm (see for instance
   http://en.wikipedia.org/wiki/Reservoir_sampling)

   TODO: a more efficient algorithm when given gen length is known *)
let multi_choice_gen n e =
  match BatGen.next e with
  | None -> BatGen.empty
  | Some x ->
    let chosen = Array.make n (x,0) in
    for i = 1 to n-1 do
      match BatGen.next e with
      | None -> failwith "Random.multi_choice"
      | Some x -> chosen.(i) <- x, i
    done;
    BatGen.iteri (fun i x ->
      let i = i + n + 1 in (* we've already chosen the n first items *)
      let r = Random.int i in
      if r < n then chosen.(r) <- x, i) e ;
    Array.sort (fun (_, i1) (_, i2) -> compare i1 i2) chosen ;
    BatArray.gen (Array.map fst chosen)

(*$T multi_choice
  BatGen.is_empty (multi_choice 0 BatGen.empty)
  BatGen.length (multi_choice 3 (BatList.gen [1;2;3;4;5])) = 3
  let l = [1;2;3;4;5] in let e = multi_choice 2 (BatList.gen l) in \
    let a = BatOption.get (BatGen.get e) in a < BatOption.get (BatGen.get e)
  let x = BatGen.repeat [0;1] |> BatGen.take 99 |> BatGen.map (fun l -> \
    multi_choice 1 (BatList.gen l)) \
      |> BatGen.map BatGen.get_exn \
      |> reduce (+) in x >= 0 && x <= 99
*)
(* Note: this last test check that the first nor the last item is always chosen *)

let shuffle_gen e =
  let a = BatArray.of_gen e in
  for n = Array.length a - 1 downto 1 do
    let k    = int ( n + 1 ) in
    if k <> n then
      let buf  = Array.get a n in
      Array.set a n (Array.get a k);
      Array.set a k buf
  done;
  a

let get_state = Random.get_state
let set_state = Random.set_state
