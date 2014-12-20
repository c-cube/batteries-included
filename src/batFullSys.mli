include module type of BatSys

val files_of: string -> string BatEnum.t
    (**As {!readdir} but the results are presented as an enumeration
       of names.*)
