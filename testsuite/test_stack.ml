open OUnit
module Gen = BatGen
module Stack = BatStack
module List = BatList

let tests = "Stack" >::: [
  "of_gen empty" >:: begin function () ->
    let e = Gen.empty in
    let s = Stack.of_gen e in
    assert_bool "stack is not empty" (Stack.is_empty s);
    assert_equal ~printer:string_of_int 0 (Stack.length s);
  end;
  "of_gen simple" >:: begin function () ->
    let e = List.gen [1;2;3] in
    let s = Stack.of_gen e in
    assert_bool "stack is empty" (not (Stack.is_empty s));
    assert_equal ~printer:string_of_int 3 (Stack.length s);
    assert_equal ~printer:string_of_int 3 (Stack.pop s);
    assert_equal ~printer:string_of_int 2 (Stack.pop s);
    assert_equal ~printer:string_of_int 1 (Stack.pop s);
    assert_raises Stack.Empty (fun () -> Stack.pop s);
  end;
  "gen empty" >:: begin function () ->
    let e = Stack.gen (Stack.create ()) in
    assert_bool "gen is not empty" (Gen.is_empty e);
  end;
  "gen nonempty" >:: begin function () ->
    let s = Stack.create () in
    Stack.push 5 s;
    Stack.push 7 s;
    assert_equal [7;5] (List.of_gen (Stack.gen s));
  end
]
