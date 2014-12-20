
open BatRandom

let enum_bits () = BatEnum.from bits
let enum_int bound = BatEnum.from (fun () -> int bound)
let enum_int32 bound = BatEnum.from (fun () -> int32 bound)
let enum_int64 bound = BatEnum.from (fun () -> int64 bound)
let enum_float bound = BatEnum.from (fun () -> float bound)
let enum_nativeint bound = BatEnum.from (fun () -> nativeint bound)
let enum_bool () = BatEnum.from bool
let enum_char () = BatEnum.from char

let choice e = choice_gen (BatEnum.gen e) 

let multi_choice n e = BatEnum.of_gen (multi_choice_gen n (BatEnum.gen e))

(*$T multi_choice
  BatEnum.is_empty (multi_choice 0 (BatEnum.empty ()))
  BatEnum.count (multi_choice 3 (BatList.enum [1;2;3;4;5])) = 3
  let l = [1;2;3;4;5] in let e = multi_choice 2 (BatList.enum l) in \
    let a = BatOption.get (BatEnum.get e) in a < BatOption.get (BatEnum.get e)
  let x = BatEnum.repeat ~times:99 [0;1] /@ (fun l -> \
    multi_choice 1 (BatList.enum l)) /@ \
    BatEnum.get_exn |> \
    reduce (+) in x > 0 && x < 99
*)
(* Note: this last test check that the first nor the last item is always chosen *)

let shuffle e =
  shuffle_gen (BatEnum.gen e)

module State = struct
  open BatRandom.State

  (**A constructor for enumerations of random numbers. *)
  let enum_bits state () = BatEnum.from (fun () -> bits state)
  let enum_int state bound = BatEnum.from (fun () -> int state bound)
  let enum_int32 state bound = BatEnum.from (fun () -> int32 state bound)
  let enum_int64 state bound = BatEnum.from (fun () -> int64 state bound)
  let enum_float state bound = BatEnum.from (fun () -> float state bound)
  let enum_nativeint state bound =
    BatEnum.from (fun () -> nativeint state bound)
  let enum_bool state () = BatEnum.from (fun () -> bool state)
  let enum_char state () = BatEnum.from (fun () -> char state)
end

module Incubator = struct
  module Private_state_enums = struct
    module State = struct
      include State (* the state we defined up above *)

      let random_enum state next =
        let rec aux state =
          let next  () = next state in
          let count () = raise BatEnum.Infinite_enum in
          let clone () = aux ( copy state ) in
          BatEnum.make ~next ~count ~clone
        in aux (copy state)

      let enum_bits state () =
        random_enum state bits

      let enum_int state bound =
        random_enum state (fun state -> int state bound)

      let enum_int32 state bound =
        random_enum state (fun state -> int32 state bound)

      let enum_int64 state bound =
        random_enum state (fun state -> int64 state bound)

      let enum_float state bound =
        random_enum state (fun state -> float state bound)

      let enum_nativeint state bound =
        random_enum state (fun state -> nativeint state bound)

      let enum_bool state () =
        random_enum state bool

      let enum_char state () =
        random_enum state char

      type implementation = { st : int array; mutable idx : int };;
      (*      external t_of_impl: implementation -> t = "%identity" *)
      external impl_of_t: t -> implementation = "%identity"

      let perturb state =
        let impl = impl_of_t state in
        make (Array.append impl.st [|impl.idx|])

    end

    (* bumps the existing global RNG state (reseeding on its current
       array) and returns the previous state *)
    let perturb_global () =
      let s_in = get_state () in
      set_state (State.perturb s_in);
      s_in

    let enum_bits () = State.enum_bits (perturb_global ()) ()
    let enum_bool () = State.enum_bool (perturb_global ()) ()
    let enum_char () = State.enum_char (perturb_global ()) ()

    let enum_int bound = State.enum_int (perturb_global ()) bound
    let enum_int32 bound = State.enum_int32 (perturb_global ()) bound
    let enum_int64 bound = State.enum_int64 (perturb_global ()) bound
    let enum_float bound = State.enum_float (perturb_global ()) bound
    let enum_nativeint bound = State.enum_nativeint (perturb_global ()) bound

  end
end
