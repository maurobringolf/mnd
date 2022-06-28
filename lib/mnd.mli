(*

MIT License

Copyright (c) [2022] [Mauro Bringolf]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*)

(** {1 Binary monads}

   A binary monad interprets a type [('a, 'b) t] as computation of values of type ['a].
   Note that combining such monads requires the second type ['b] to be equal in all of them.
*)

module type MONAD2_DEF = sig
  type ('a, 'b) t

  val return : 'a -> ('a, 'b) t
  val map : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
  val bind : ('a1, 'b) t -> ('a1 -> ('a2, 'b) t) -> ('a2, 'b) t
end

module type MONAD2 = sig
  include MONAD2_DEF

  val ( let* ) : ('a1, 'b) t -> ('a1 -> ('a2, 'b) t) -> ('a2, 'b) t
  val ( let+ ) : ('a1, 'b) t -> ('a1 -> 'a2) -> ('a2, 'b) t
  val ( >>= ) : ('a1, 'b) t -> ('a1 -> ('a2, 'b) t) -> ('a2, 'b) t
  val ( >> ) : ('a1, 'b) t -> ('a2, 'b) t -> ('a2, 'b) t
  val ( |>> ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  val foldM : ('a2 -> 'a1 -> ('a2, 'b) t) -> 'a2 -> 'a1 list -> ('a2, 'b) t
  val fold1M : ('a -> 'a -> ('a, 'b) t) -> 'a list -> ('a, 'b) t
  val iterM : ('a -> (unit, 'b) t) -> 'a list -> (unit, 'b) t
  val forM : 'a list -> ('a -> ('b, 'c) t) -> ('b list, 'c) t
  val mapM : ('a -> ('b, 'e) t) -> 'a list -> ('b list, 'e) t
  val ifM : bool -> (unit -> (unit, 'e) t) -> (unit, 'e) t
end

module Make2 : functor (M : MONAD2_DEF) ->
  MONAD2 with type ('a, 'b) t = ('a, 'b) M.t

(** {1 Unary monads}
   A unary monad interprets a type ['a t] as computation of values of type ['a].
*)

(**  The fundamental functions defining a monad, i.e. what you need to implement in order to instantiate a monad with {!Make} *)
module type MONAD_DEF = sig
  type 'a t

  val return : 'a -> 'a t
  (** [return x] should be a monad which produces [x] *)

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
  (** Run a monad and apply a function to the result *)

  val bind : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t
  (** Sequential composition: First run one monad, then another based on the first result *)
end

(** The monad interface with [let*]-syntax, common operators and functions. These are the elements you get when instantiating a monad with {!Make}.*)
module type MONAD = sig
  include MONAD_DEF

  val ( let+ ) : 'a1 t -> ('a1 -> 'a2) -> 'a2 t
  (** This is just syntactic sugar for [map]: [let+ x = m in f x] is the same as [map f m]. *)

  val ( let* ) : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t
  (** This is just syntactic sugar for [bind]: [let* x = m in f x] is the same as [bind m f]. *)

  val ( >>= ) : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t
  (** Operator version of [bind], i.e. [m1 >>= m2] is the same as [bind m1 m2] *)

  val ( >> ) : 'a1 t -> 'a2 t -> 'a2 t
  (** Like [(>>=)] but ignores the result of the first monad, i.e. [m1 >> m2] is the same as [m1 >>= fun _ -> m2] *)

  val ( |>> ) : 'a t -> ('a -> 'b) -> 'b t
  (** This is the operator version of [map], i.e. [m |>> f] is the same as [map f m] *)

  val foldM : ('a2 -> 'a1 -> 'a2 t) -> 'a2 -> 'a1 list -> 'a2 t
  (** Monadic version of [List.fold_left]: the folding function computes the new accumulator value inside the monad. *)

  val fold1M : ('a -> 'a -> 'a t) -> 'a list -> 'a t
  (** Same as [foldM] but the first element of the list is taken as the initial accumulator value. Throws an [Invalid_argument] exception on the empty list. *)

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  (** Monadic version of [List.map]: the new elements are computed inside the monad. *)

  val forM : 'a list -> ('a -> 'b t) -> 'b list t
  (** This is just {!mapM} with its arguments flipped. *)

  val iterM : ('a -> unit t) -> 'a list -> unit t
  (** Monadic version of [List.iter] *)

  val ifM : bool -> (unit -> unit t) -> unit t
  (** Conditionally run the given monad or just return [()] *)
end

(** Functor to instantiate the [MONAD] interface for custom monads by providing a [MONAD_DEF]. *)
module Make : functor (M : MONAD_DEF) -> MONAD with type 'a t = 'a M.t

module Instances : sig
  module Option : MONAD with type 'a t = 'a option

  module Result : sig
    include MONAD2 with type ('a, 'b) t = ('a, 'b) result

    val error : 'b -> ('a, 'b) t
  end

  module State : sig
    include MONAD2 with type ('a, 'b) t = 'b -> 'b * 'a

    val get : ('b, 'b) t
    val put : 'b -> (unit, 'b) t
    val run : 'b -> ('a, 'b) t -> 'a
  end

  module Reader : sig
    include MONAD2 with type ('a, 'b) t = 'b -> 'a

    val read : ('b, 'b) t
    val run : 'b -> ('a, 'b) t -> 'a
  end

  module type MONOID = sig
    type t

    val mempty : t
    val mappend : t -> t -> t
  end

  module Writer : functor (M : MONOID) -> sig
    include MONAD with type 'a t = 'a * M.t

    val write : M.t -> unit * M.t
  end
end
