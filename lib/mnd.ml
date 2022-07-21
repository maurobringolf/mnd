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

module type MONAD2_WITH_AND = sig
  include MONAD2

  val ( and+ ) : ('a1, 'b) t -> ('a2, 'b) t -> ('a1 * 'a2, 'b) t
  val ( and* ) : ('a1, 'b) t -> ('a2, 'b) t -> ('a1 * 'a2, 'b) t
end

module Make2 (M : MONAD2_DEF) : MONAD2 with type ('a, 'b) t = ('a, 'b) M.t =
struct
  include M

  let ( let* ) = M.bind
  let ( let+ ) x f = M.map f x
  let ( >>= ) = M.bind
  let ( >> ) x y = x >>= fun _ -> y
  let ( |>> ) x y = M.map y x

  let foldM f acc xs =
    List.fold_left (fun acc x -> acc >>= fun a -> f a x) (return acc) xs

  let fold1M f = function
    | [] -> invalid_arg "fold1M of empty list"
    | x :: xs -> foldM f x xs

  let rec mapM m xs =
    match xs with
    | [] -> return []
    | x :: xs ->
        let* y = m x in
        let* ys = mapM m xs in
        return (y :: ys)

  let forM xs m = mapM m xs
  let iterM m xs = mapM m xs >> return ()
  let ifM cond m = if cond then m () else return ()
end

module Make2WithProduct (M : sig
  include MONAD2_DEF

  val product : ('a1, 'b) t -> ('a2, 'b) t -> ('a1 * 'a2, 'b) t
end) =
struct
  include Make2 (M)

  let ( and+ ) = M.product
  let ( and* ) = M.product
end

module type MONAD_DEF = sig
  type 'a t

  val return : 'a -> 'a t
  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
  val bind : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t
end

(** The monad interface with [let*]-syntax, common operators and functions. These are the elements you get when instantiating a monad with {!Make}.*)
module type MONAD = sig
  include MONAD_DEF

  val ( let+ ) : 'a1 t -> ('a1 -> 'a2) -> 'a2 t
  val ( let* ) : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t
  val ( >>= ) : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t
  val ( >> ) : 'a1 t -> 'a2 t -> 'a2 t
  val ( |>> ) : 'a t -> ('a -> 'b) -> 'b t
  val foldM : ('a2 -> 'a1 -> 'a2 t) -> 'a2 -> 'a1 list -> 'a2 t
  val fold1M : ('a -> 'a -> 'a t) -> 'a list -> 'a t
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val forM : 'a list -> ('a -> 'b t) -> 'b list t
  val iterM : ('a -> unit t) -> 'a list -> unit t
  val ifM : bool -> (unit -> unit t) -> unit t
end

(** Functor to instantiate the [MONAD] interface for custom monads by providing a [MONAD_DEF]. *)
module Make (M : MONAD_DEF) = struct
  include Make2 (struct
    include M

    type ('a, 'b) t = 'a M.t
  end)

  type 'a t = 'a M.t
end

module Instances = struct
  module Option = Make (struct
    type 'a t = 'a option

    let bind = Option.bind
    let map = Option.map
    let return = Option.some
  end)

  module Result = struct
    include Make2WithProduct (struct
      type ('a, 'b) t = ('a, 'b) Stdlib.Result.t

      let bind = Stdlib.Result.bind
      let map = Stdlib.Result.map
      let return = Stdlib.Result.ok

      let product r1 r2 =
        match (r1, r2) with
        | Ok x1, Ok x2 -> Ok (x1, x2)
        | Ok _, Error e | Error e, _ -> Error e
    end)

    let error = Stdlib.Result.error
  end

  module State = struct
    include Make2 (struct
      type ('a, 'b) t = 'b -> 'b * 'a

      let bind m1 m2 s =
        let s1, x = m1 s in
        m2 x s1

      let map f m s =
        let s, r = m s in
        (s, f r)

      let return x s = (s, x)
    end)

    let run (init : 'b) (m : ('a, 'b) t) : 'a = snd (m init)
    let get : ('b, 'b) t = fun s -> (s, s)
    let put (s : 'b) : (unit, 'b) t = fun _ -> (s, ())
  end

  module Reader = struct
    include Make2 (struct
      type ('a, 'b) t = 'b -> 'a

      let bind m1 m2 s = m2 (m1 s) s
      let map f m s = f (m s)
      let return x _ = x
    end)

    let read : ('b, 'b) t = fun e -> e
    let run (init : 'b) (m : ('a, 'b) t) : 'a = m init
  end

  module type MONOID = sig
    type t

    val mempty : t
    val mappend : t -> t -> t
  end

  module Writer (M : MONOID) = struct
    include Make (struct
      type 'a t = 'a * M.t

      let bind (x, y1) m =
        let r, y2 = m x in
        (r, M.mappend y1 y2)

      let map f (x, y) = (f x, y)
      let return x = (x, M.mempty)
    end)

    let write (y : M.t) = ((), y)
  end
end
