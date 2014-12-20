
include (BatArray : module type of BatArray with module Cap := BatArray.Cap)

type 'a enumerable = 'a t

let enum xs =
  let rec make start xs =
    let n = length xs in
    (* inside the loop, as [make] may later be called with another array *)
    BatEnum.make
      ~next:(fun () ->
        if !start < n then
          xs.(BatRef.post_incr start)
        else
          raise BatEnum.No_more_elements)
      ~count:(fun () ->
        n - !start)
      ~clone:(fun () ->
        make (BatRef.copy start) xs)
  in
  make (ref 0) xs
(*$Q enum
  (Q.array Q.small_int) (fun a -> \
    let e = enum a in \
    for i = 0 to Array.length a / 2 - 1 do\
      assert (a.(i) = BatEnum.get_exn e)\
    done; \
    let e' = BatEnum.clone e in \
    assert (BatEnum.count e = BatEnum.count e'); \
    for i = Array.length a / 2 to Array.length a - 1 do \
      assert (a.(i) = BatEnum.get_exn e && a.(i) = BatEnum.get_exn e') \
    done; \
    BatEnum.is_empty e && BatEnum.is_empty e' \
  )
*)


let of_enum e =
  let n = BatEnum.count e in
  (* This assumes, reasonably, that init traverses the array in order. *)
  Array.init n
    (fun _i ->
      match BatEnum.get e with
      | Some x -> x
      | None -> assert false (*BISECT-VISIT*))

let backwards xs =
  let rec make start xs =
    BatEnum.make
      ~next:(fun () ->
        if !start > 0 then
          xs.(BatRef.pre_decr start)
        else
          raise BatEnum.No_more_elements)
      ~count:(fun () ->
        !start)
      ~clone:(fun () ->
        make (BatRef.copy start) xs)
  in
  make (ref (length xs)) xs
(*$Q backwards
  (Q.array Q.small_int) (fun a -> \
    let e = backwards a in \
    let n = Array.length a in \
    for i = 0 to Array.length a / 2 - 1 do\
      assert (a.(n - 1 - i) = BatEnum.get_exn e)\
    done; \
    let e' = BatEnum.clone e in \
    assert (BatEnum.count e = BatEnum.count e'); \
    for i = Array.length a / 2 to Array.length a - 1 do \
      assert (a.(n - 1 - i) = BatEnum.get_exn e && \
              a.(n - 1 - i) = BatEnum.get_exn e') \
    done; \
    BatEnum.is_empty e && BatEnum.is_empty e' \
  )
*)

let of_backwards e =
  let a = of_enum e in
  rev_in_place a;
  a

let range xs = BatEnum.(--^) 0 (Array.length xs)

let print ?(first="[|") ?(last="|]") ?(sep="; ") print_a  out t =
  match length t with
  | 0 ->
    BatInnerIO.nwrite out first;
    BatInnerIO.nwrite out last
  | n ->
    BatInnerIO.nwrite out first;
    print_a out (unsafe_get t 0);
    for i = 1 to n - 1 do
      BatInnerIO.nwrite out sep;
      print_a out (unsafe_get t i);
    done;
    BatInnerIO.nwrite out last
(*$T
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) \
    [|2;4;66|] = "[2,4,66]"
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) \
    [|2|] = "[2]"
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) \
    [||] = "[]"
*)


module Cap = struct
  include BatArray.Cap
  let enum         = enum
  let of_enum      = of_enum
  let backwards    = backwards
  let of_backwards = of_backwards
  let print        = print
end
