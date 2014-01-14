(*
 * BatList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
 * Copyright (C) 2008 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(** Interface to FileUtils *)

module Path : sig
  type t = FilePath.filename

  val compare : t -> t -> int

  val concat : t -> t -> t

  val print : 'a BatIO.output -> t -> unit

  val pwd : unit -> t

  val stat : t -> FileUtil.stat

  val kind : t -> FileUtil.kind
end

module Size : sig
  type t = FileUtil.size

  val compare : t -> t -> int

  val add : t -> t -> t
  
  val print : 'a BatIO.output -> t -> unit
  (** Human-friendly printing *)

  val to_bytes : t -> int64
  (** Conversion into bytes *)

  val to_string : ?fuzzy:bool -> t -> string
  (** Print the size into a string. If fuzzy is true (default:false) an
      approximation is returned. *)
end

val find : ?follow:FileUtil.action_link ->
            FileUtil.test_file ->
            Path.t ->
            (Path.t * FileUtil.kind) list
(** Find file in a subdirectory, and return a list of files together with
    their kind. A predicate on files is to be provided.
    @param follow: behavior for symbolic links *)

val find_all : ?follow:FileUtil.action_link ->
                Path.t ->
                (Path.t * FileUtil.kind) list
(** Shortcut for [find], with no filter *)
