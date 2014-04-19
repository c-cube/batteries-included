open OUnit
open BatPervasives

let assert_equal_arrays =
  assert_equal ~printer:(BatIO.to_string (BatArray.print BatInt.print))

let take_array n e = BatArray.of_gen (BatGen.take n e)

let test_gen_helper reset create modify =
  let make n = take_array n (create ()) in

  (* Generations constructed for the same state should be equal. *)
  let () = reset () in
  let a = make 10 in
  let () = reset () in
  let b = make 10 in
  let () = assert_equal_arrays a b in

  (* The states should be shared: if the state is modified then the second
     stream should be different. *)
  let () = reset () in
  let a = make 1000 in
  let () = reset () in
  let () = modify () in
  let b = make 1000 in
  let () = assert_bool "Different states but equal arrays" (a <> b) in

  ()

(* Wrapper that assures that [cmd] does not modify the default state. *)
let with_saved_state cmd =
  let state = BatRandom.get_state () in
  let () = cmd () in
    BatRandom.set_state state

let test_gen_default () =
  let reset () = BatRandom.init 0 in
  let create () = BatRandom.gen_int 100 in
  let modify () = let _ = BatRandom.int 100 in () in
    with_saved_state
      (fun () -> test_gen_helper reset create modify)

let test_gen_state () =
  let make_seed () = BatRandom.State.make [| 0 |] in
  let state = ref (make_seed ()) in
  let reset () = state := make_seed () in
  let create () = BatRandom.State.gen_int !state 100 in
  let modify () = let _ = BatRandom.State.int !state 100 in () in
    test_gen_helper reset create modify

module PSE = BatRandom.Incubator.Private_state_gens

let test_gen_default_priv () =
  let reset () = BatRandom.init 0 in
  let create () = PSE.gen_int 100 in
  let modify () = let _ = BatRandom.int 100 in () in
  with_saved_state (fun () -> test_gen_helper reset create modify)

let test_gen_state_priv () =
  let make_seed () = BatRandom.State.make [| 0 |] in
  let state = ref (make_seed ()) in
  let reset () = state := make_seed () in
  let create () = PSE.State.gen_int !state 100 in
  let modify () = let _ = PSE.State.int !state 100 in () in
  test_gen_helper reset create modify


let tests = "BatRandom" >::: [
  "gen_default" >:: test_gen_default;
  "gen_state" >:: test_gen_state;
  "gen_default_priv" >:: test_gen_default_priv;
  "gen_state_priv" >:: test_gen_state_priv;
]
