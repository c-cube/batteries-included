include BatInternalBitSet

let enum t =
  let rec make n cnt =
    let cur = ref n in
    let cnt = ref cnt in
    let rec next () =
      match next_set_bit t !cur with
        Some elem ->
        decr cnt;
        cur := (elem+1);
        elem
      | None ->
        raise BatEnum.No_more_elements
    in
    BatEnum.make
      ~next
      ~count:(fun () -> !cnt)
      ~clone:(fun () -> make !cur !cnt)
  in
  make 0 (count t)

(*$T
  BitSet.of_list [5;3;2;1] |> BitSet.enum |> Enum.skip 1 |> Enum.count = 3
  let e = BitSet.of_list [5;3;2;1] |> enum in \
    Enum.junk e; Enum.iter (fun _ -> ()) (Enum.clone e); (Enum.count e = 3)
*)

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let b = BitSet.of_list (List.map abs l) in \
    b |> BitSet.enum |> BitSet.of_enum |> equal b)
*)

let of_enum ?(cap=128) e = let bs = create cap in BatEnum.iter (set bs) e; bs

