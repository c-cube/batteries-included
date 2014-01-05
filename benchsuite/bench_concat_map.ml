(* cd .. && ocamlbuild benchsuite/bench_concat_map.native *)

(* compare Enum.concat_map f and Enum.(map f %> concat) *)

let (--) = BatEnum.(--)

let naive f e = BatEnum.flatten (BatEnum.map f e)
let concat_map f e = BatEnum.concat_map f e

let f bind x = bind (fun x -> x -- 10) (x -- (x+3))

let test_bind bind arg =
  let n = (1 -- arg) |> bind (f bind) |> BatEnum.sum in (* evaluate the enum, bind is lazy *)
  ()

let bench_concat_map n =
  Bench.bench_arg
    [ "naive_" ^ string_of_int n, (test_bind naive), n
    ; "concat_map_" ^ string_of_int n, (test_bind concat_map), n
    ]

let () =
  List.iter
    (fun n ->
      let res = bench_concat_map n in
      Bench.summarize res;
      print_endline ""
    ) [ 10_000; 50_000; 100_000 ]
