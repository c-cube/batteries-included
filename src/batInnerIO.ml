(*
 * BatInnerIO - Abstract input/output (inner module)
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 Philippe Strauss
 *               2008 David Teller
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


type 'a weak_set = ('a, unit) BatInnerWeaktbl.t
let weak_create size     = BatInnerWeaktbl.create size
let weak_add set element = BatInnerWeaktbl.add set element ()
let weak_iter f s        = BatInnerWeaktbl.iter (fun x _ -> f x) s

module type IO_MONAD = sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
  val fail : exn -> 'a m
  val sync : ('a -> 'b) -> 'a -> 'b
end

let lock = ref BatConcurrent.nolock

module Sequential_IO_Monad : IO_MONAD with type 'a m = 'a = struct
  type 'a m = 'a

  let bind x f = f x
  let return x = x
  let fail e = raise e

  let sync f x = BatConcurrent.sync !lock f x
end

module type S = sig
  type 'a m = 'a IO_Monad.m

  type input
  type 'a output

  exception No_more_input
  (** This exception is raised when reading on an input with the [read] or
      [nread] functions while there is no available token to read. *)

  exception Input_closed
  (** This exception is raised when reading on a closed input. *)

  exception Output_closed
  (** This exception is raised when reading on a closed output. *)

  val read : input -> char m
  (** Read a single char from an input or raise [No_more_input] if
      no input available. *)

  val read_all : input -> string m
  (** read all the contents of the input until [No_more_input] is raised. *)

  val pipe : unit -> input * unit output
  (** Create a pipe between an input and an ouput. Data written from
      the output can be read from the input. *)

  val nread : input -> int -> string m
  (** [nread i n] reads a string of size up to [n] from an input.
      The function will raise [No_more_input] if no input is available.
      It will raise [Invalid_argument] if [n] < 0. *)

  val really_nread : input -> int -> string m
  (** [really_nread i n] reads a string of exactly [n] characters
      from the input. @raise No_more_input if at least [n] characters are
      not available. @raise Invalid_argument if [n] < 0. *)

  val input : input -> string -> int -> int -> int m
  (** [input i s p l] reads up to [l] characters from the given input, storing
      them in string [s], starting at character number [p]. It returns the actual
      number of characters read or raise [No_more_input] if no character can be
      read. It will raise [Invalid_argument] if [p] and [l] do not designate a
      valid substring of [s]. *)

  val really_input : input -> string -> int -> int -> int m
  (** [really_input i s p l] reads exactly [l] characters from the given input,
      storing them in the string [s], starting at position [p]. For consistency with
      {!BatIO.input} it returns [l]. @raise No_more_input if at [l] characters are
      not available. @raise Invalid_argument if [p] and [l] do not designate a
      valid substring of [s]. *)

  val close_in : input -> unit m
  (** Close the input. It can no longer be read from. *)

  (*val auto_close_in : input -> input
    (** Create a new channel which will close automatically once there is nothing
      left to read.*)*)

  val write : 'a output -> char -> unit m
  (** Write a single char to an output. *)

  val nwrite : 'a output -> string -> unit m
  (** Write a string to an output. *)

  val output : 'a output -> string -> int -> int -> int m
  (** [output o s p l] writes up to [l] characters from string [s], starting at
      offset [p]. It returns the number of characters written. It will raise
      [Invalid_argument] if [p] and [l] do not designate a valid substring of [s]. *)

  val really_output : 'a output -> string -> int -> int -> int m
  (** [really_output o s p l] writes exactly [l] characters from string [s] onto
      the the output, starting with the character at offset [p]. For consistency with
      {!BatIO.output} it returns [l]. @raise Invalid_argument if [p] and [l] do not
      designate a valid substring of [s]. *)

  val flush : 'a output -> unit m
  (** Flush an output. *)

  val flush_all : unit -> unit m
  (** Flush all outputs. *)

  val close_out : 'a output -> 'a m
  (** Close the output and return its accumulator data.
      It can no longer be written. *)

  val close_all : unit -> unit m
  (** Close all outputs.
      Ignore errors.*)

  val input_string : string -> input
  (** Create an input that will read from a string. *)

  val output_string : unit -> string output
  (** Create an output that will write into a string in an efficient way.
      When closed, the output returns all the data written into it. *)

  val on_close_out : 'a output -> ('a output -> unit) -> unit
  (**
     Register a function to be triggered just before an output is closed.
  *)

  val create_in :
    read:(unit -> char m) ->
    input:(string -> int -> int -> int m) ->
    close:(unit -> unit m) -> input
  (** Fully create an input by giving all the needed functions.

      {b Note} Do {e not} use this function for creating an input
      which reads from one or more underlying inputs. Rather, use
      {!wrap_in}.
  *)

  val inherit_in:
    ?read:(unit -> char m) ->
    ?input:(string -> int -> int -> int m) ->
    ?close:(unit -> unit m) ->
    input -> input
  (**
     Simplified and optimized version of {!wrap_in} whenever only
     one input appears as dependency.
  *)


  val wrap_in :
    read:(unit -> char m) ->
    input:(string -> int -> int -> int m) ->
    close:(unit -> unit m) ->
    underlying:(input list) ->
    input
  (** Fully create an input reading from other inputs by giving all the needed functions.

      This function is a more general version of {!create_in}
      which also handles dependency management between inputs.
  *)




  val create_out :
    write:(char -> unit m) ->
    output:(string -> int -> int -> int m) ->
    flush:(unit -> unit m) ->
    close:(unit -> 'a m) ->
    'a output
  (**
      Fully create an output by giving all the needed functions.

      @param write  Write one character to the output (see {!write}).
      @param output Write a (sub)string to the output (see {!output}).
      @param flush  Flush any buffers of this output  (see {!flush}).
      @param close  Close this output. The output will be automatically
      flushed.

      {b Note} Do {e not} use this function for creating an output which
      writes to one or more underlying outputs. Rather, use {!wrap_out}.
  *)

  val inherit_out:
    ?write:(char -> unit m) ->
    ?output:(string -> int -> int -> int m) ->
    ?flush:(unit -> unit m) ->
    ?close:(unit -> unit m) ->
    _ output -> unit output
  (**
     Simplified and optimized version of {!wrap_out} whenever only
     one output appears as dependency.
  *)


  val wrap_out :
    write:(char -> unit m)      ->
    output:(string -> int -> int -> int m) ->
    flush:(unit -> unit m)      ->
    close:(unit -> 'a m)        ->
    underlying:('b output list) ->
    'a output
  (**
     Fully create an output that writes to one or more underlying outputs.

     This function is a more general version of {!create_out},
     which also handles dependency management between outputs.

     To illustrate the need for dependency management, let us consider
     the following values:
     - an output [out]
     - a function [f : _ output -> _ output], using {!create_out} to
     create a new output for writing some data to an underyling
     output (for instance, a function comparale to {!tab_out} or a
     function performing transparent compression or transparent
     traduction between encodings)

     With these values, let us consider the following scenario
     - a new output [f out] is created
     - some data is written to [f out] but not flushed
     - output [out] is closed, perhaps manually or as a consequence
     of garbage-collection, or because the program has ended
     - data written to [f out] is flushed.

     In this case, data reaches [out] only after [out] has been closed,
     which violates the protocol.  Despite appearances, it is quite easy
     to reach such situation, especially in short programs.

     The solution is to use [wrap_out] rather than [create_out] in [f].
     Specifying that [f out] writes on [out] will then let the run-time
     flush and close [f out] when [out] is closed for any reason, which
     in turn avoids the issue.

     @param write  Write one character to the output (see {!write}).
     @param output Write a (sub)string to the output (see {!output}).
     @param flush  Flush any buffers of this output  (see {!flush}).
     @param close  Close this output. The output will be automatically
     flushed.
     @param underlying The list of outputs to which the new output will
     write.

     {b Note} Function [close] should {e not} close [underlying]
     yourself. This is a common mistake which may cause sockets or
     standard output to be closed while they are still being used by
     another part of the program.
  *)

  val default_buffer_size : int
  (**The default size of buffers.*)


  (** {6 Binary files API}

          Here is some API useful for working with binary files, in particular
          binary files generated by C applications. By default, encoding of
          multibyte integers is low-endian. The BigEndian module provide multibyte
          operations with other encoding.
  *)

  exception Overflow of string
  (** Exception raised when a read or write operation cannot be completed. *)

  val read_byte : input -> int m
  (** Read an unsigned 8-bit integer. *)

  val read_signed_byte : input -> int m
  (** Read an signed 8-bit integer. *)

  val read_ui16 : input -> int m
  (** Read an unsigned 16-bit word. *)

  val read_i16 : input -> int m
  (** Read a signed 16-bit word. *)

  val read_i32 : input -> int m
  (** Read a signed 32-bit integer. @raise Overflow if the
      read integer cannot be represented as an OCaml 31-bit integer. *)

  val read_real_i32 : input -> int32 m
  (** Read a signed 32-bit integer as an OCaml int32. *)

  val read_i64 : input -> int64 m
  (** Read a signed 64-bit integer as an OCaml int64. *)

  val read_float : input -> float m
  (** Read an IEEE single precision floating point value. *)

  val read_double : input -> float m
  (** Read an IEEE double precision floating point value. *)

  val read_string : input -> string m
  (** Read a null-terminated string. *)

  val read_line : input -> string m
  (** Read a LF or CRLF terminated string. *)

  val write_byte : 'a output -> int -> unit m
  (** Write an unsigned 8-bit byte. *)

  val write_ui16 : 'a output -> int -> unit m
  (** Write an unsigned 16-bit word. *)

  val write_i16 : 'a output -> int -> unit m
  (** Write a signed 16-bit word. *)

  val write_i32 : 'a output -> int -> unit m
  (** Write a signed 32-bit integer. *)

  val write_real_i32 : 'a output -> int32 -> unit m
  (** Write an OCaml int32. *)

  val write_i64 : 'a output -> int64 -> unit m
  (** Write an OCaml int64. *)

  val write_double : 'a output -> float -> unit m
  (** Write an IEEE double precision floating point value. *)

  val write_float : 'a output -> float -> unit m
  (** Write an IEEE single precision floating point value. *)

  val write_string : 'a output -> string -> unit m
  (** Write a string and append an null character. *)

  val write_line : 'a output -> string -> unit m
  (** Write a line and append a LF (it might be converted
          to CRLF on some systems depending on the underlying BatIO). *)

  (* XXX not true anymore.
  external cast_output : 'a output -> unit output = "%identity"
  (** You can safely transform any output to an unit output in a safe way
      by using this function. *)
  *)

  val cast_output : 'a output -> unit output

  (** {6 Comparison}

      The following modules may be useful to create hashtables of inputs or outputs.
  *)

  module Input :
  sig
    type t = input
    val compare : input -> input -> int
    (**A total order on inputs*)

    val hash    : input -> int
    (**A hash function for inputs*)

    val equal : input -> input -> bool
  end

  module Output :
  sig
    type t = unit output
    val compare : _ output -> _ output -> int
    (**A total order on outputs*)

    val hash    : _ output -> int
    (**A hash function for outputs*)

    val equal : _ output -> _ output -> bool
  end

  (**/**)
  (**{6 Internals}*)

  external noop        : unit      -> unit        = "%ignore"
  val noop_m : unit -> unit m

  (**
     {7 Optimized access to fields}
  *)

  val get_output : _ output -> (string -> int -> int -> int m)
  val get_flush  : _ output -> (unit -> unit m)

  (**
     {7 Facilities for debugging}
  *)

  val get_output_id : _ output -> int
  val get_input_id  : input -> int

  (**/**)
end

module Make(M : IO_MONAD) : S with module IO_Monad = M = struct

  module IO_Monad = M

  type 'a m = 'a IO_Monad.m

  let (>>=) = IO_Monad.bind

  type input = {
    mutable in_read  : unit -> char m;
    mutable in_input : string -> int -> int -> int m;
    mutable in_close : unit -> unit m;
    in_id: int;(**A unique identifier.*)
    in_upstream: input weak_set
  }

  type 'a output = {
    mutable out_write : char -> unit m;
    mutable out_output: string -> int -> int -> int m;
    mutable out_close : unit -> 'a m;
    mutable out_flush : unit -> unit m;
    out_id:    int;(**A unique identifier.*)
    out_upstream:unit output weak_set
      (** The set of outputs which have been created to write to this output.*)
  }


  module Input =
  struct
    type t = input
    let compare x y = x.in_id - y.in_id
    let hash    x   = x.in_id
    let equal   x y = x.in_id = y.in_id
  end

  module Output =
  struct
    type t = unit output
    let compare x y = x.out_id - y.out_id
    let hash    x   = x.out_id
    let equal   x y = x.out_id = y.out_id
  end


  (**All the currently opened outputs -- used to permit [flush_all] and [close_all].*)
  (*module Inputs = Weaktbl.Make(Input)*)
  module Outputs= Weak.Make(Output)


  (** {6 Primitive operations}*)

  external noop        : unit      -> unit        = "%ignore"

  let return_unit = IO_Monad.return ()
  let noop_m () = return_unit

  (* XXX depreciate
  external cast_output : 'a output -> unit output = "%identity"
  *)

  let cast_output o =
    let out_close () = o.out_close () >>= fun _ -> return_unit in
    { o with out_close; }

  let outputs = Outputs.create 32
  let outputs_add out =
    IO_Monad.sync (Outputs.add outputs) out

  let outputs_remove out =
    IO_Monad.sync (Outputs.remove outputs) out


  exception No_more_input
  exception Input_closed
  exception Output_closed




  let post_incr r =
    let result = !r in
    incr r;
    result
  let post r op =
    let result = !r in
    r := op !r;
    result

  let uid = ref 0
  let uid () = post_incr uid

  let on_close_out out f =
    BatConcurrent.sync !lock (fun () ->
      let do_close = out.out_close in
      out.out_close <- (fun () -> f out; do_close ())) ()

  let on_close_in inp f =
    BatConcurrent.sync !lock (fun () ->
      let do_close = inp.in_close in
      inp.in_close <- (fun () -> f inp; do_close ())) ()

  let close_in i =
    i.in_close() >>= fun () ->
    i.in_read <- (fun _ -> IO_Monad.fail Input_closed);
    i.in_input <- (fun _ _ _ -> IO_Monad.fail Input_closed);
    i.in_close <- noop_m;  (*Double closing is not a problem*)
    return_unit


  let wrap_in ~read ~input ~close ~underlying =
    let result =
      {
        in_read     = read;
        in_input    = input;
        in_close    = close;
        in_id       = uid ();
        in_upstream = weak_create 2
      }
    in
    IO_Monad.sync (List.iter (fun x -> weak_add x.in_upstream result)) underlying;
    Gc.finalise (fun i -> ignore (close_in i)) result;
    result

  let inherit_in ?read ?input ?close inp =
    let read  = match read  with None -> inp.in_read | Some f -> f
    and input = match input with None -> inp.in_input| Some f -> f
    and close = match close with None -> noop_m      | Some f -> f
    in  wrap_in ~read ~input ~close ~underlying:[inp]


  let create_in ~read ~input ~close =
    wrap_in ~read ~input ~close ~underlying:[]

  (*For recursively closing outputs, we need either polymorphic
    recursion or a hack. Well, a hack it is.*)

  (*Close a [unit output] -- note that this works for any kind of output,
    thanks to [cast_output], but this can't return a proper result.*)
  let rec close_unit (o:unit output) : unit m =
    let forbidden _ = raise Output_closed in
    o.out_flush () >>= fun () ->
    (* XXX should use `Traverse.fold_m` later if blocking is needed *)
    weak_iter (fun o -> ignore (close_unit o)) o.out_upstream;
    o.out_close() >>= fun r ->
    o.out_write  <- forbidden;
    o.out_output <- forbidden;
    o.out_close  <- (fun _ -> IO_Monad.return r) (*Closing again is not a problem*);
    o.out_flush  <- noop_m;   (*Flushing again is not a problem*);
    return_unit

  (*Close a ['a output] -- first close it as a [unit output] then
    recover the result.*)
  let close_out o =
    (*  Printf.eprintf "close_out\n%!";*)
    close_unit (cast_output o);
    o.out_close ()


  let ignore_close_out out = ignore (close_out out)

  let wrap_out ~write ~output ~flush ~close ~underlying  =
    let rec out =
      {
        out_write  = write;
        out_output = output;
        out_close  = (fun () ->
          outputs_remove (cast_output out);
          close ());
        out_flush  = flush;
        out_id     = uid ();
        out_upstream = weak_create 2
      }
    in
    let o = cast_output out in
    BatConcurrent.sync !lock (List.iter (fun x -> weak_add x.out_upstream o)) underlying;
    outputs_add (cast_output out);
    Gc.finalise ignore_close_out out;
    out

  let inherit_out ?write ?output ?flush ?close out =
    let write = match write  with None -> out.out_write | Some f -> f
    and output= match output with None -> out.out_output| Some f -> f
    and flush = match flush  with None -> out.out_flush | Some f -> f
    and close = match close  with None -> ignore        | Some f -> f
    in wrap_out ~write ~output ~flush ~close ~underlying:[out]

  let create_out ~write ~output ~flush ~close =
    wrap_out ~write ~output ~flush ~close ~underlying:[]

  let read i = i.in_read()

  let nread i n =
    if n < 0 then invalid_arg "BatIO.nread";
    if n = 0 then
      ""
    else
      let s = String.create n in
      let l = ref n in
      let p = ref 0 in
      try
        while !l > 0 do
          let r = i.in_input s !p !l in
          if r = 0 then raise No_more_input;
          p := !p + r;
          l := !l - r;
        done;
        s
      with
        No_more_input as e ->
        if !p = 0 then raise e;
        String.sub s 0 !p

  let really_output o s p l' =
    let sl = String.length s in
    if p + l' > sl || p < 0 || l' < 0 then invalid_arg "BatIO.really_output";
    let l = ref l' in
    let p = ref p in
    while !l > 0 do
      let w = o.out_output s !p !l in
      if w = 0 then raise Sys_blocked_io;
      p := !p + w;
      l := !l - w;
    done;
    l'

  let input i s p l =
    let sl = String.length s in
    if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.input";
    if l = 0 then
      0
    else
      i.in_input s p l

  let really_input i s p l' =
    let sl = String.length s in
    if p + l' > sl || p < 0 || l' < 0 then invalid_arg "BatIO.really_input";
    let l = ref l' in
    let p = ref p in
    while !l > 0 do
      let r = i.in_input s !p !l in
      if r = 0 then raise Sys_blocked_io;
      p := !p + r;
      l := !l - r;
    done;
    l'

  let really_nread i n =
    if n < 0 then invalid_arg "BatIO.really_nread";
    if n = 0 then ""
    else
      let s = String.create n
      in
      ignore(really_input i s 0 n);
      s


  let write o x = o.out_write x

  let nwrite o s =
    let p = ref 0 in
    let l = ref (String.length s) in
    while !l > 0 do
      let w = o.out_output s !p !l in
      (* FIXME: unknown how many characters were already written *)
      if w = 0 then raise Sys_blocked_io;
      p := !p + w;
      l := !l - w;
    done

  let output o s p l =
    let sl = String.length s in
    if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.output";
    o.out_output s p l

  let flush o = o.out_flush()

  let flush_all () =
    BatConcurrent.sync !lock ( Outputs.iter (fun o -> try flush o with _ -> ())) outputs

  let close_all () =
    let outs =
      BatConcurrent.sync !lock (Outputs.fold (fun o os -> o :: os) outputs) []
    in
    List.iter (fun o -> try close_out o with _ -> ()) outs

  let read_all i =
    let maxlen = 1024 in
    let str    = ref [] in
    let pos    = ref 0 in
    let rec loop() =
      let s = nread i maxlen in
      str := (s,!pos) :: !str;
      pos := !pos + String.length s;
      loop()
    in
    try
      loop()
    with
      No_more_input
    | Input_closed ->
      let buf = String.create !pos in
      List.iter (fun (s,p) ->
        String.unsafe_blit s 0 buf p (String.length s)
      ) !str;
      buf

  let input_string s =
    let pos = ref 0 in
    let len = String.length s in
    create_in
      ~read:(fun () ->
        if !pos >= len then raise No_more_input
        else String.unsafe_get s (post_incr pos))
      ~input:(fun sout p l ->
        if !pos >= len then raise No_more_input;
        let n = (if !pos + l > len then len - !pos else l) in
        String.unsafe_blit s (post pos ( (+) n ) ) sout p n;
        n
      )
      ~close:noop



  (**
     {6 Standard BatIO}
  *)




  let default_buffer_size = 16 (*Arbitrary number. If you replace it, just
                                                         don't put something too small, i.e. anything
                                                         smaller than 10 is probably a bad idea.*)

  let output_string() =
    let b = Buffer.create default_buffer_size in
    create_out
      ~write:  (fun c -> Buffer.add_char b c )
      ~output: (fun s p l -> Buffer.add_substring b s p l;  l  )
      ~close:  (fun () -> Buffer.contents b)
      ~flush:  noop


  let pipe() =
    let input = ref "" in
    let inpos = ref 0 in
    let output = Buffer.create default_buffer_size in
    let flush() =
      input := Buffer.contents output;
      inpos := 0;
      Buffer.reset output;
      if String.length !input = 0 then raise No_more_input
    in
    let read() =
      if !inpos = String.length !input then flush();
      String.unsafe_get !input (post_incr inpos)
    in
    let input s p l =
      if !inpos = String.length !input then flush();
      let r = (if !inpos + l > String.length !input then String.length !input - !inpos else l) in
      String.unsafe_blit !input !inpos s p r;
      inpos := !inpos + r;
      r
    in
    let write c =
      Buffer.add_char output c
    in
    let output s p l =
      Buffer.add_substring output s p l;
      l
    in
    let input  = create_in ~read ~input  ~close:noop
    and output = create_out ~write ~output ~close:noop ~flush:noop
    in
    input , output



  (*let to_input_channel inp =
    let (fin, fout) = Unix.pipe () in
      let outp = out_channel fout  in
      (*connect [inp] to [outp]*)
      in_channel_of_descr fin*)
  (**
     {6 Binary APIs}
  *)


  exception Overflow of string

  let read_byte i = int_of_char (i.in_read())

  let read_signed_byte i =
    let c = int_of_char (i.in_read()) in
    if c land 128 <> 0 then
      c - 256
    else
      c

  let read_string i =
    let b = Buffer.create 8 in
    let rec loop() =
      let c = i.in_read() in
      if c <> '\000' then begin
        Buffer.add_char b c;
        loop();
      end;
    in
    loop();
    Buffer.contents b

  let read_line i =
    let b = Buffer.create 80 in
    let cr = ref false in
    let rec loop() =
      match i.in_read() with
      | '\n' ->
        ()
      | '\r' when !cr ->
        Buffer.add_char b '\r';
        loop()
      | '\r' ->
        cr := true;
        loop()
      | c when !cr ->
        cr := false;
        Buffer.add_char b '\r';
        Buffer.add_char b c;
        loop();
      | c ->
        Buffer.add_char b c;
        loop()
    in
    try
      loop();
      Buffer.contents b
    with
      No_more_input ->
      if !cr then Buffer.add_char b '\r';
      if Buffer.length b > 0 then
        Buffer.contents b
      else
        raise No_more_input

  (*$= read_line & ~cmp:BatString.equal ~printer:String.quote
    "abc" (read_line (BatIO.input_string "abc\ndef\n"))
    "abc" (read_line (BatIO.input_string "abc\r\ndef\n"))
    "abc\r" (read_line (BatIO.input_string "abc\r\r\ndef\n"))
    "abc" (read_line (BatIO.input_string "abc"))
    "abc\r" (read_line (BatIO.input_string "abc\r"))
    "kldsjf\r\r\rasdfa"  (read_line (BatIO.input_string "kldsjf\r\r\rasdfa\nsfdsagf\n"))
  *)

  let read_ui16 i =
    let ch1 = read_byte i in
    let ch2 = read_byte i in
    ch1 lor (ch2 lsl 8)

  let read_i16 i =
    let ch1 = read_byte i in
    let ch2 = read_byte i in
    let n = ch1 lor (ch2 lsl 8) in
    if ch2 land 128 <> 0 then
      n - 65536
    else
      n

  let fix = lnot 0x7FFFFFFF (* -:) *)

  let read_i32 ch =
    let ch1 = read_byte ch in
    let ch2 = read_byte ch in
    let ch3 = read_byte ch in
    let ch4 = read_byte ch in
    if ch4 land 128 <> 0 then begin
      if ch4 land 64 = 0 then raise (Overflow "read_i32");
      (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)) lor fix (* FIX HERE *)
    end else begin
      if ch4 land 64 <> 0 then raise (Overflow "read_i32");
      ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
    end

  let read_real_i32 ch =
    let ch1 = read_byte ch in
    let ch2 = read_byte ch in
    let ch3 = read_byte ch in
    let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
    let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
    Int32.logor base big

  let read_i64 ch =
    let ch1 = read_byte ch in
    let ch2 = read_byte ch in
    let ch3 = read_byte ch in
    let ch4 = read_byte ch in
    let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
    let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
    let big = Int64.of_int32 (read_real_i32 ch) in
    Int64.logor (Int64.shift_left big 32) small

  let read_double ch =
    Int64.float_of_bits (read_i64 ch)

  let read_float ch =
    Int32.float_of_bits (read_real_i32 ch)

  let write_byte o n =
    (* doesn't test bounds of n in order to keep semantics of Pervasives.output_byte *)
    write o (Char.unsafe_chr (n land 0xFF))

  let write_string o s =
    nwrite o s;
    write o '\000'

  let write_line o s =
    nwrite o s;
    write o '\n'

  let write_ui16 ch n =
    if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
    write_byte ch n;
    write_byte ch (n lsr 8)

  let write_i16 ch n =
    if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
    if n < 0 then
      write_ui16 ch (65536 + n)
    else
      write_ui16 ch n

  let write_i32 ch n =
    write_byte ch n;
    write_byte ch (n lsr 8);
    write_byte ch (n lsr 16);
    write_byte ch (n asr 24)

  let write_real_i32 ch n =
    let base = Int32.to_int n in
    let big = Int32.to_int (Int32.shift_right_logical n 24) in
    write_byte ch base;
    write_byte ch (base lsr 8);
    write_byte ch (base lsr 16);
    write_byte ch big

  let write_i64 ch n =
    write_real_i32 ch (Int64.to_int32 n);
    write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32))

  let write_double ch f =
    write_i64 ch (Int64.bits_of_float f)

  let write_float ch f =
    write_real_i32 ch (Int32.bits_of_float f)

  let get_output out = out.out_output
  let get_flush  out = out.out_flush

  let get_output_id out = out.out_id
  let get_input_id  inp = inp.in_id
end

include Make(Sequential_IO_Monad)

(** A placeholder used to allow recursive use of [self]
    in an [input_channel]*)
let placeholder_in =
  { in_read  = (fun () -> ' ');
    in_input = (fun _ _ _ -> 0);
    in_close = noop;
    in_id    = (-1);
    in_upstream= weak_create 0 }
let input_channel ?(autoclose=true) ?(cleanup=false) ch =
  let me = ref placeholder_in (*placeholder*)
  in let result =
    create_in
      ~read:(fun () -> try input_char ch
      with End_of_file ->
        if autoclose then close_in !me;
        raise No_more_input)
      ~input:(fun s p l ->
        let n = Pervasives.input ch s p l in
        if n = 0 then
          begin
            if autoclose then close_in !me else ();
            raise No_more_input
          end
        else n)
      ~close:(if cleanup then fun () -> Pervasives.close_in ch else ignore)
  in
  me := result;
  result

let output_channel ?(cleanup=false) ch =
  create_out
    ~write: (fun c     -> output_char ch c)
    ~output:(fun s p l -> Pervasives.output ch s p l; l)
    ~close: (if cleanup then fun () ->
        begin
          (*		   Printf.eprintf "Cleaning up\n%!";*)
          Pervasives.close_out ch
        end
      else fun () ->
        begin
          (*		   Printf.eprintf "Not cleaning up\n%!";*)
          Pervasives.flush ch
        end)
    ~flush: (fun ()    -> Pervasives.flush ch)


let stdin  = input_channel Pervasives.stdin
let stdout = output_channel Pervasives.stdout
let stderr = output_channel Pervasives.stderr
let stdnull= create_out
    ~write:ignore
    ~output:(fun _ _ l -> l)
    ~flush:ignore
    ~close:ignore

