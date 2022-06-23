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
module Make (M : MONAD_DEF) : MONAD with type 'a t := 'a M.t = struct
  include Make2 (struct
    include M

    type ('a, 'b) t = 'a M.t
  end)
end

module Instances = struct
  module Option = Make (struct
    type 'a t = 'a option

    let bind = Option.bind
    let map = Option.map
    let return = Option.some
  end)

  module Result = struct
    include Make2 (struct
      type ('a, 'b) t = ('a, 'b) Stdlib.Result.t

      let bind = Stdlib.Result.bind
      let map = Stdlib.Result.map
      let return = Stdlib.Result.ok
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
