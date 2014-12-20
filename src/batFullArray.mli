
include module type of BatArray with module Cap := BatArray.Cap

val enum : 'a array -> 'a BatEnum.t
(** Returns an enumeration of the elements of an array.
    Behavior of the enumeration is undefined if the contents of the array changes afterwards.*)

val of_enum : 'a BatEnum.t -> 'a array
(** Build an array from an enumeration. *)

val backwards : 'a array -> 'a BatEnum.t
(** Returns an enumeration of the elements of an array, from last to first. *)

val of_backwards : 'a BatEnum.t -> 'a array
(** Build an array from an enumeration, with the first element of
    the enumeration as the last element of the array and vice
    versa. *)


val range : 'a array -> int BatEnum.t
(** [range a] returns an enumeration of all valid indexes into the given
    array.  For example, [range [|2;4;6;8|] = 0--3].*)

(** {6 Boilerplate code}*)

val print : ?first:string -> ?last:string -> ?sep:string ->
  ('a, 'b) BatIO.printer -> ('a t, 'b) BatIO.printer
(** Print the contents of an array, with [~first] preceeding the first
    item (default: "\[|"), [~last] following the last item (default:
    "|\]") and [~sep] separating items (default: "; ").  A printing
    function must be provided to print the items in the array.

    Example: IO.to_string (Array.print Int.print) [|2;4;66|] = "[|2; 4; 66|]"
*)


(** {5 Capabilities for arrays.}

    This modules provides the same set of features as {!Array}, but
    with the added twist that arrays can be made read-only or
    write-only.  Read-only arrays may then be safely shared and
    distributed.

    There is no loss of performance involved.
*)
module Cap :
sig
  (**
     Only the capability-specific functions are documented here.
     See the complete [Array] module for the documentation of other functions.
  *)

  type ('a, 'b) t constraint 'b = [< `Read | `Write]
  (**The type of arrays with capabilities.
     An [('a, [`Read | `Write])] array behaves as a regular ['a array],
     while a [('a, [`Read]) array] only has read-only capabilities
     and a [('a, [`Write]) array] only has write-only capabilities.*)

  (**{6 Base operations}*)

  external length : ('a, [> ]) t -> int = "%array_length"
  external get : ('a, [> `Read]) t -> int -> 'a = "%array_safe_get"
  external set : ('a, [> `Write]) t -> int -> 'a -> unit = "%array_safe_set"

  (**{6 Constructors}*)

  external make : int -> 'a -> ('a, _) t = "caml_make_vect"
  external create : int -> 'a -> ('a, _) t = "caml_make_vect"

##V<4.2##  val make_float : int -> (float, _) t
##V>=4.2##  external make_float : int -> (float, _) t = "caml_make_float_vect"
(** [Array.make_float n] returns a fresh float array of length [n],
    with uninitialized data.

    @since 2.3.0 and OCaml 4.2.0
 *)

  external of_array  : 'a array -> ('a, _ ) t = "%identity"
  (** Adopt a regular array as a capability array, allowing
    to decrease capabilities if necessary.

    This operation involves no copying. In other words, in
    [let cap = of_array a in ...], any modification in [a]
    will also have effect on [cap] and reciprocally.*)

  external to_array  : ('a, [`Read | `Write]) t -> 'a array = "%identity"
  (** Return a capability array as an array.

    This operation requires both read and write permissions
    on the capability array and involves no copying. In other
    words, in [let a = of_array cap in ...], any modification
    in [a] will also have effect on [cap] and reciprocally.*)

  external read_only :  ('a, [>`Read])  t -> ('a, [`Read])  t = "%identity"
  (** Drop to read-only permissions.

    This operation involves no copying.*)

  external write_only : ('a, [>`Write]) t -> ('a, [`Write]) t = "%identity"
  (** Drop to write-only permissions.

    This operation involves no copying.*)

  val init : int -> (int -> 'a) -> ('a, _) t
  val make_matrix : int -> int -> 'a -> (('a, _)t, _) t
  val create_matrix : int -> int -> 'a ->  (('a, _)t, _) t

  (** {6 Iterators}*)

  val iter : ('a -> unit) -> ('a, [> `Read]) t -> unit
  val map : ('a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
  val iteri : (int -> 'a -> unit) -> ('a, [> `Read]) t -> unit
  val mapi : (int -> 'a -> 'b) -> ('a, [> `Read]) t -> ('b, _) t
  val modify : ('a -> 'a) -> ('a, [`Read | `Write]) t -> unit
  val modifyi : (int -> 'a -> 'a) -> ('a, [`Read | `Write]) t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, [> `Read]) t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> ('b, [> `Read]) t -> 'a -> 'a

  (**{6 Operations on two arrays}*)

  val iter2 : ('a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit
  val iter2i : (int -> 'a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit

  (**{6 Predicates}*)

  val for_all : ('a -> bool) -> ('a, [> `Read]) t -> bool
  val exists : ('a -> bool) -> ('a, [> `Read]) t -> bool
  val find : ('a -> bool) -> ('a, [> `Read]) t -> 'a
  val mem : 'a -> ('a, [> `Read]) t -> bool
  val memq : 'a -> ('a, [> `Read]) t -> bool
  val findi : ('a -> bool) -> ('a, [> `Read]) t -> int
  val filter : ('a -> bool) -> ('a, [> `Read]) t -> ('a, _) t
  val filter_map : ('a -> 'b option) -> ('a, [> `Read]) t -> ('b, _) t
  val find_all : ('a -> bool) -> ('a, [> `Read]) t -> ('a, _) t
  val partition : ('a -> bool) -> ('a, [> `Read]) t -> ('a, _) t * ('a, _)t

  (** {6 Array transformations} *)

  val rev : ('a, [> `Read]) t -> ('a, _) t
  val rev_in_place : ('a, [`Read | `Write]) t -> unit
  val append : ('a, [> `Read]) t ->  ('a, [> `Read]) t -> ('a, _) t
  val concat : ('a, [> `Read]) t list -> ('a, _) t
  val sub : ('a, [> `Read]) t -> int -> int -> ('a, _) t
  val copy : ('a, [> `Read]) t -> 'a array
  val fill : ('a, [> `Write]) t -> int -> int -> 'a -> unit
  val blit : ('a, [> `Read]) t -> int -> ('a, [>`Write]) t -> int -> int -> unit

  (** {6 Conversions} *)

  val gen : ('a, [> `Read]) t -> 'a BatGen.t
  val of_gen : 'a BatGen.t -> ('a, _) t
  val gen_backwards : ('a, [> `Read]) t -> 'a BatGen.t
  val of_gen_backwards : 'a BatGen.t -> ('a, _) t
  val to_list : ('a, [> `Read]) t -> 'a list
  val of_list : 'a list -> ('a, _) t

  val enum : ('a, [> `Read]) t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a, _) t
  val backwards : ('a, [> `Read]) t -> 'a BatEnum.t
  val of_backwards : 'a BatEnum.t -> ('a, _) t

  (** {6 Utilities} *)

  val sort : ('a -> 'a -> int) -> ('a, [> `Read | `Write]) t -> unit
  val stable_sort : ('a -> 'a -> int) -> ('a, [ `Read | `Write]) t -> unit
  val fast_sort : ('a -> 'a -> int) -> ('a, [`Read | `Write]) t -> unit

  (** {6 Boilerplate code}*)

  val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatIO.output -> 'b -> unit) ->  'a BatIO.output -> ('b, [>`Read]) t -> unit

  val compare : 'a BatOrd.comp -> ('a, [> `Read]) t BatOrd.comp
  val ord : 'a BatOrd.ord -> ('a, [> `Read]) t BatOrd.ord
  val equal : 'a BatOrd.eq -> ('a, [> `Read]) t BatOrd.eq

  (** {6 Override modules}*)

  (** Operations on {!BatArray.Cap} without exceptions.*)
  module Exceptionless : sig
    val find : ('a -> bool) -> ('a, [> `Read]) t -> 'a option
    val findi : ('a -> bool) -> ('a, [> `Read]) t -> int option
  end

  (** Operations on {!BatArray.Cap} with labels. *)
  module Labels : sig
    val init : int -> f:(int -> 'a) -> ('a, _) t
    val make: int -> init:'a -> ('a, _) t
    val create: int -> init:'a -> ('a, _) t
    val make_matrix : dimx:int -> dimy:int -> 'a -> (('a, _)t, _) t
    val create_matrix : dimx:int -> dimy:int -> 'a -> (('a, _)t, _) t
    val sub : ('a, [> `Read]) t -> pos:int -> len:int -> ('a, _) t
    val fill : ('a, [> `Write]) t -> pos:int -> len:int -> 'a -> unit
    val blit : src:('a, [> `Read]) t -> src_pos:int -> dst:('a, [>`Write]) t ->
      dst_pos:int -> len:int -> unit
    val iter : f:('a -> unit) -> ('a, [> `Read]) t -> unit
    val map : f:('a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
    val iteri : f:(int -> 'a -> unit) -> ('a, [> `Read]) t -> unit
    val mapi : f:(int -> 'a -> 'b) -> ('a, [> `Read]) t -> ('b, _) t
    val modify : f:('a -> 'a) -> ('a, [`Read | `Write]) t -> unit
    val modifyi : f:(int -> 'a -> 'a) -> ('a, [`Read | `Write]) t -> unit
    val fold_left : f:('a -> 'b -> 'a) -> init:'a ->  ('b, [> `Read]) t -> 'a
    val fold_right : f:('b -> 'a -> 'a) -> ('b, [> `Read]) t -> init:'a -> 'a
    val sort : cmp:('a -> 'a -> int) -> ('a, [> `Read | `Write]) t -> unit
    val stable_sort : cmp:('a -> 'a -> int) -> ('a, [ `Read | `Write]) t -> unit
    val fast_sort : cmp:('a -> 'a -> int) -> ('a, [`Read | `Write]) t -> unit
    val iter2:      f:('a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit
    val iter2i:     f:( int -> 'a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit
    val exists:     f:('a -> bool) -> ('a, [> `Read]) t -> bool
    val for_all:    f:('a -> bool) -> ('a, [> `Read]) t -> bool
    val find:       f:('a -> bool) -> ('a, [> `Read]) t -> 'a
    val map:        f:('a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
    val mapi:       f:(int -> 'a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
    val filter:     f:('a -> bool) -> ('a, [>`Read]) t -> ('a, _) t
    val filter_map: f:('a -> 'b option) -> ('a, [>`Read]) t -> ('b, _) t
  end
  (**/**)
  (** {6 Undocumented functions} *)

  external unsafe_get : ('a, [> `Read]) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a, [> `Write])t -> int -> 'a -> unit = "%array_unsafe_set"

  (**/**)
end

