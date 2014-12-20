include module type of BatInt
  with module Infix := BatInt.Infix
  and module Safe_int := BatInt.Safe_int

val ( -- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5 -- 10] is the enumeration 5,6,7,8,9,10.
    [10 -- 5] is the empty enumeration*)

val ( --- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5 --- 10] is the enumeration 5,6,7,8,9,10.
    [10 --- 5] is the enumeration 10,9,8,7,6,5.*)

module Infix : BatFullNumber.Infix with type bat__infix_t = t

(** {7 Printing}*)

val print: 'a BatInnerIO.output -> int -> unit
(** prints as decimal string *)

val print_hex: 'a BatInnerIO.output -> int -> unit
(** prints as hex string *)

(*    val bprint: 'a BatInnerIO.output -> t -> unit
      (** prints as binary string *) *)

module Safe_int : sig
  include module type of BatInt.Safe_int
    with module Infix := BatInt.Safe_int.Infix
  module Infix : BatFullNumber.Infix with type bat__infix_t = t

  val print: 'a BatInnerIO.output -> t -> unit
end
