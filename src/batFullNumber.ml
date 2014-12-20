
include (BatNumber : (module type of BatNumber
  with module type Infix := BatNumber.Infix
  and module type Discrete := BatNumber.Discrete
  and module type Numeric := BatNumber.Numeric))

module type Infix = sig
  include BatNumber.Infix
  val ( -- ): bat__infix_t -> bat__infix_t -> bat__infix_t BatEnum.t
  val ( --- ): bat__infix_t -> bat__infix_t -> bat__infix_t BatEnum.t
end

module type Discrete = sig
  include BatNumber.Discrete
  val ( -- ): discrete -> discrete -> discrete BatEnum.t
  val ( --- ): discrete -> discrete -> discrete BatEnum.t
end

module type Numeric = sig
  include module type of BatNumber.Numeric
    with module Infix := BatNumber.Numeric.Infix

  module Infix : Infix
  include Infix with type bat__infix_t = t
end
