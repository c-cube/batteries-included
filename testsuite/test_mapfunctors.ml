open OUnit
open BatRandom
open BatPervasives

module MkTest (MkMap : functor (Ord : BatInterfaces.OrderedType)
                 -> BatMap.S with type key = Ord.t) =
struct
  (* This is basically Test_pmap, but specialized for MkMap(Int) *)
  module Map = MkMap (BatInt)

  let print_gen out gen =
    BatGen.print begin
      fun out (c, _) ->
        BatPrintf.fprintf out "%d" c
    end out gen

  let assert_equal_gens gen_1 gen_2 =
    match BatGen.compare ~cmp:compare (gen_1 ()) (gen_2 ()) with
      | 0 -> (* pass *) ()
      | _ ->
          assert_failure
            (BatPrintf.sprintf2 "Expected %a, got %a"
               print_gen (gen_1 ()) print_gen (gen_2 ()))

  let assert_equal_maps map_1 map_2 =
    let gen_1 () = Map.gen map_1 in
    let gen_2 () = Map.gen map_2 in
    assert_equal_gens gen_1 gen_2

  let gen_map state bound count =
    let keys = BatGen.take count (State.gen_int state bound) in
    Map.of_gen (BatGen.map (fun x -> (x, x)) keys)

  let test_traversal_order () =
    let init = State.make [|0|] in
    let map = gen_map init 10 50 in
    let gen_1 () = Map.gen map
    and gen_2 () =
      let list = BatRefList.empty () in
      Map.iter (fun k v -> BatRefList.push list (k, v)) map;
      BatRefList.backwards list
    in
    assert_equal_gens gen_1 gen_2

  let tests = [
    "traversal order iter vs. gen" >:: test_traversal_order ;
  ]
end

let tests =
  let module MT1 = MkTest (BatMap.Make) in
  let mt1_tests = "Map.Make" >::: MT1.tests in
  let module MT2 = MkTest (BatSplay.Map) in
  let mt2_tests = "Splay.Make" >::: MT2.tests in
  "Generic Map tests" >::: [
    mt1_tests ;
    mt2_tests ;
  ]
