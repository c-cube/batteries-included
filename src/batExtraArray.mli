
open BatArray

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

module Cap : sig
  open BatArray.Cap

  val enum : ('a, [> `Read]) t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a, _) t
  val backwards : ('a, [> `Read]) t -> 'a BatEnum.t
  val of_backwards : 'a BatEnum.t -> ('a, _) t

  val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatIO.output -> 'b -> unit) ->  'a BatIO.output -> ('b, [>`Read]) t -> unit

end
