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


module Path = struct

  type t = FilePath.filename

  let compare = FilePath.compare

  let concat = FilePath.concat

  let print = BatIO.write_string

  let pwd () = FileUtil.pwd ()

  let stat f = FileUtil.stat f

  let kind f =
    let s = stat f in
    s.FileUtil.kind
end

module Size = struct
  type t = FileUtil.size

  let compare = FileUtil.size_compare ~fuzzy:false

  let add = FileUtil.size_add

  let to_string = FileUtil.string_of_size

  let print o s = BatIO.write_string o (to_string s)

  let to_bytes s = FileUtil.byte_of_size s
end

let find ?follow test fln =
  FileUtil.find ?follow test fln
    (fun acc f -> (f, Path.kind f) :: acc) []

let find_all ?follow fln = find ?follow FileUtil.True fln
