
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




module Cap =
struct
  (** Implementation note: in [('a, 'b) t], ['b] serves only as
      a phantom type, to mark which operations are only legitimate on
      readable arrays or writeable arrays.*)
  type ('a, 'b) t = 'a array constraint 'b = [< `Read | `Write]

  external of_array   : 'a array -> ('a, _ ) t                  = "%identity"
  external to_array   : ('a, [`Read | `Write]) t -> 'a array    = "%identity"
  external read_only  : ('a, [>`Read])  t -> ('a, [`Read])  t   = "%identity"
  external write_only : ('a, [>`Write]) t -> ('a, [`Write]) t   = "%identity"
  external length     : ('a, [> ]) t -> int                     = "%array_length"
  external get        : ('a, [> `Read]) t -> int -> 'a          = "%array_safe_get"
  external set        : ('a, [> `Write]) t -> int -> 'a -> unit = "%array_safe_set"
  external make       : int -> 'a -> ('a, _) t                  = "caml_make_vect"
  external create     : int -> 'a -> ('a, _) t                  = "caml_make_vect"


##V>=4.2##  external make_float: int -> (float, _) t = "caml_make_float_vect"
##V<4.2##  let make_float n = make n 0.

  let init         = init
  let make_matrix  = make_matrix
  let create_matrix= create_matrix
  let iter         = iter
  let map          = map
  let filter       = filter
  let filter_map   = filter_map
  let iteri        = iteri
  let mapi         = mapi
  let modify       = modify
  let modifyi      = modifyi
  let fold_left    = fold_left
  let fold_right   = fold_right
  let iter2        = iter2
  let iter2i       = iter2i
  let for_all      = for_all
  let exists       = exists
  let find         = find
  let mem          = mem
  let memq         = memq
  let findi        = findi
  let find_all     = find_all
  let partition    = partition
  let rev          = rev
  let rev_in_place = rev_in_place
  let append       = append
  let concat       = concat
  let sub          = sub
  let copy         = copy
  let fill         = fill
  let blit         = blit
  let enum         = enum
  let of_enum      = of_enum
  let backwards    = backwards
  let of_backwards = of_backwards
  let gen          = gen
  let of_gen       = of_gen
  let gen_backwards    = gen_backwards
  let of_gen_backwards = of_gen_backwards
  let to_list      = to_list
  let of_list      = of_list
  let sort         = sort
  let stable_sort  = stable_sort
  let fast_sort    = fast_sort
  let print        = print
  let compare      = compare
  let ord          = ord
  let equal        = equal
  external unsafe_get : ('a, [> `Read]) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a, [> `Write])t -> int -> 'a -> unit = "%array_unsafe_set"

  (*BISECT-IGNORE-BEGIN*)
  module Labels =
  struct
    let init i ~f = init i f
    let create len ~init = create len init
    let make = create
    let make_matrix ~dimx ~dimy x = make_matrix dimx dimy x
    let create_matrix = make_matrix
    let sub a ~pos ~len = sub a pos len
    let fill a ~pos ~len x = fill a pos len x
    let blit ~src ~src_pos ~dst ~dst_pos ~len = blit src src_pos dst dst_pos len
    let iter ~f a = iter f a
    let map ~f a = map  f a
    let iteri ~f a = iteri f a
    let mapi ~f a = mapi f a
    let modify ~f a = modify f a
    let modifyi ~f a = modifyi f a
    let fold_left ~f ~init a = fold_left f init a
    let fold_right ~f a ~init= fold_right f a init
    let sort ~cmp a = sort cmp a
    let stable_sort ~cmp a = stable_sort cmp a
    let fast_sort ~cmp a = fast_sort cmp a
    let iter2 ~f a b = iter2 f a b
    let exists ~f a  = exists f a
    let for_all ~f a = for_all f a
    let iter2i  ~f a b = iter2i f a b
    let find ~f a = find f a
    let filter ~f a = filter f a
    let filter_map ~f a = filter_map f a
  end

  module Exceptionless =
  struct
    let find f e =
      try Some (find f e)
      with Not_found -> None

    let findi f e =
      try Some (findi f e)
      with Not_found -> None
  end
  (*BISECT-IGNORE-END*)
end
