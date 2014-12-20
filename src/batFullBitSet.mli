include module type of BatBitSet

val enum : t -> int BatEnum.t
(** [enum s] returns an enumeration of bits which are set
    in the bitset [s]. *)

val of_enum : ?cap:int -> int BatEnum.t -> t
(** [of_enum ~cap e] builds a bitset of capacity [cap] an enumeration
    of ints [e].

    Note: Performance of this function may be poor if enumeration is
    in increasing order and the max.
*)
