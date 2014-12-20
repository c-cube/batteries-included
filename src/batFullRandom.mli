include module type of BatRandom with module State := BatRandom.State

(** {6 Enumerations of random values.} *)

val enum_bits  : unit    -> int BatEnum.t

val enum_int   : int     -> int BatEnum.t

val enum_bool  : unit    -> bool BatEnum.t

val enum_float : float   -> float BatEnum.t

val enum_int32 : Int32.t -> Int32.t BatEnum.t

val enum_int64 : Int64.t -> Int64.t BatEnum.t

val enum_nativeint : Nativeint.t -> Nativeint.t BatEnum.t

val enum_char : unit -> char BatEnum.t

(*val enum_uchar : unit -> UChar.t BatEnum.t*)

val choice : 'a BatEnum.t -> 'a
(** [choice e] returns a randomly-chosen element of [e].

    This function only works on finite enumerations with
    less than 2{^30} elements.*)

val multi_choice : int -> 'a BatEnum.t -> 'a BatEnum.t
(** [multi_choice n e] returns an enumeration of [n]
    randomly-chosen elements of [e]. *)

val shuffle: 'a BatEnum.t -> 'a array
(** [shuffle e] returns a new array, containing the
    same set of elements as [e], but in a random order.

    Shuffling is implemented using the Fisher-Yates
    algorithm and works in O(n), where n is the number
    of elements of [e].

    This function only works on finite enumerations with
    less than 2{^30} elements. *)

module State : sig
  include module type of BatRandom.State

  val enum_bits  : t -> unit    -> int BatEnum.t
  val enum_int   : t -> int     -> int BatEnum.t
  val enum_bool  : t -> unit    -> bool BatEnum.t
  val enum_float : t -> float   -> float BatEnum.t
  val enum_int32 : t -> Int32.t -> Int32.t BatEnum.t
  val enum_int64 : t -> Int64.t -> Int64.t BatEnum.t
  val enum_nativeint : t -> Nativeint.t -> Nativeint.t BatEnum.t
  val enum_char  : t -> unit    -> char BatEnum.t
      (*  val enum_uchar : t -> unit    -> UChar.t BatEnum.t*)

end

module Incubator : sig
  module Private_state_enums : sig
    module State : sig (** same as BatRandom.State *)
      type t = Random.State.t
      (** The type of PRNG states. *)

      val make : int array -> t
      (** Create a new state and initialize it with the given seed. *)

      val make_self_init : unit -> t
      (** Create a new state and initialize it with a system-dependent
          low-entropy seed. *)

      val copy : t -> t
      (** Return a copy of the given state. *)

      val bits       : t -> int
      val int        : t -> int -> int
      val int32      : t -> Int32.t -> Int32.t
      val nativeint  : t -> Nativeint.t -> Nativeint.t
      val int64      : t -> Int64.t -> Int64.t
      val float      : t -> float -> float
      val bool       : t -> bool
      val char       : t -> char

      (** A copy of the input state is made to start these generators;
          the input state is not modified.  This means that two enums
          constructed from the same state will produce the same value
          sequence. *)
      val enum_bits  : t -> unit    -> int BatEnum.t
      val enum_int   : t -> int     -> int BatEnum.t
      val enum_bool  : t -> unit    -> bool BatEnum.t
      val enum_float : t -> float   -> float BatEnum.t
      val enum_int32 : t -> Int32.t -> Int32.t BatEnum.t
      val enum_int64 : t -> Int64.t -> Int64.t BatEnum.t
      val enum_nativeint : t -> Nativeint.t -> Nativeint.t BatEnum.t
      val enum_char  : t -> unit    -> char BatEnum.t

      (** [perturb s] returns a new state based on the given state
          that is, in a sense, the hash of the input state.  This new
          state should be quite different from the input. *)
      val perturb : t -> t

    end

    (** These enumerations are built on a copy of the global RNG
        state.  To keep successive constructions from using the same RNG
        state, when any of these functions is called, the global RNG state
        is perturbed by using its current internal state as seed to
        construct a new state. *)
    val enum_bits  : unit    -> int BatEnum.t
    val enum_int   : int     -> int BatEnum.t
    val enum_bool  : unit    -> bool BatEnum.t
    val enum_float : float   -> float BatEnum.t
    val enum_int32 : Int32.t -> Int32.t BatEnum.t
    val enum_int64 : Int64.t -> Int64.t BatEnum.t
    val enum_nativeint : Nativeint.t -> Nativeint.t BatEnum.t
    val enum_char : unit -> char BatEnum.t

  end
end
