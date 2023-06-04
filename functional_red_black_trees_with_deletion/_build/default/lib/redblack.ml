(** This is how Standard ML does comparisons, which I find makes
    them much clearer than ocaml's implementation (1 for greater, 
    0 for equal, -1 for less) *)
type order = GREATER | EQUAL | LESS

module type SET = sig
    type 'a t

    (** [insert x s] is the result of adding [x] to [s]*)
    val insert: 'a -> 'a t -> 'a t

    (** [mem x s] is true iff [x] is a member of [s]*)
    val mem: 'a -> 'a t -> bool

    (** [delete x s] is the result of removing [x] from [s].
        Returns a value equal to [s] if [x] is not in [s] *)
    val delete: 'a -> 'a t -> 'a t

    (** [size s] is the number of elements in [s] *)
    val size: 'a t -> int
end

module type KEY = sig
    type key
    val compare: key -> key -> order
end
