(** This is how Standard ML does comparisons, which I find makes
    them much clearer than ocaml's implementation (1 for greater, 
    0 for equal, -1 for less) *)
type order = GREATER | EQUAL | LESS

module type SET = sig
    type key
    type t

    (** [insert x s] is the result of adding [x] to [s]*)
    val insert: key -> t -> t

    (** [mem x s] is true iff [x] is a member of [s]*)
    val mem: key -> t -> bool

    (** [delete x s] is the result of removing [x] from [s].
        Returns a value equal to [s] if [x] is not in [s] *)
    val delete: key -> t -> t

    (** [size s] is the number of elements in [s] *)
    val size: t -> int
end

module type KEY = sig
    type t
    val compare: t -> t -> order
end

module IntKey = struct
    type t = int
    
    let compare x y = 
        let res = Stdlib.compare x y in
        if res = 1 then GREATER
        else if res = 0 then EQUAL
        else LESS
end

module RedBlackSet (Key: KEY) = struct
    type key = Key.t

    type color = NegativeBlack | Red | Black | DoubleBlack 

    exception ColorArithmeticError of string
    let incr  = function
        | NegativeBlack -> Red
        | Red -> Black
        | Black -> DoubleBlack
        | DoubleBlack -> raise (ColorArithmeticError "Can't add to DoubleBlack")

    let decr  = function
        | NegativeBlack -> raise (ColorArithmeticError "Can't subtract from NegativeBlack")
        | Red -> NegativeBlack
        | Black -> Red
        | DoubleBlack -> Black

    type t = Lf of color | Br of color * key * t * t

    let tree_incr = function 
        | Lf c -> Lf (incr c)
        | Br (c, x, l, r) -> Br (incr c, x, l, r) 

    let tree_decr = function 
        | Lf c -> Lf (decr c)
        | Br (c, x, l, r) -> Br (decr c, x, l, r) 


    let rec mem x = function
        | Lf _ -> false
        | Br (_, y, t1, t2) ->
                match Key.compare x y with
                | GREATER -> mem x t1
                | EQUAL -> true
                | LESS -> mem x t2

    let rec size = function
        | Lf _ -> 0
        | Br (_, _, t1, t2) -> 1 + size t1 + size t2

    let balance t =
        match t with
        (* Handle local invariant violations caused by insertion Red nodes*)
        | Br (Black, z, Br (Red, y, Br (Red, x, t1, t2), t3), t4)
        | Br (Black, z, Br (Red, x, t1, Br (Red, y, t2, t3)), t4)
        | Br (Black, x, t1, Br (Red, z, Br (Red, y, t2, t3), t4))
        | Br (Black, x, t1, Br (Red, y, t2, Br (Red, z, t3, t4)))
            -> Br (Red, y, Br (Black, x, t1, t2), Br (Black, z, t3, t4))

        (* Handle local invariant violations caused by propagating blacks
           blacks up the tree. Note these could be collapsed with the above cases 
           if we called decr on the root color, but I'm not sure I understand well
           enough if / why this would be more perspicuous, so I'll just leave them
           as separate cases.*)
        | Br (DoubleBlack, z, Br (Red, y, Br (Red, x, t1, t2), t3), t4)
        | Br (DoubleBlack, z, Br (Red, x, t1, Br (Red, y, t2, t3)), t4)
        | Br (DoubleBlack, x, t1, Br (Red, z, Br (Red, y, t2, t3), t4))
        | Br (DoubleBlack, x, t1, Br (Red, y, t2, Br (Red, z, t3, t4)))
            -> Br (Black, y, Br (Black, x, t1, t2), Br (Black, z, t3, t4))

        (* Get rid of NegativeBlacks caused by propagating double blacks 
           up the tree *)
        | Br (DoubleBlack, z,
                Br (NegativeBlack, x,
                        Br (Black, w, t1, t2),
                        Br (Black, y, t3, t4)),
                t5) -> 
                    Br (Black, y,
                            Br (Black, x,
                                    Br (Red, w, t1, t2),
                                    t3),
                            Br (Black, z, t4, t5)
                        )
        | Br (DoubleBlack, w,
                t1,
                Br (NegativeBlack, y,
                        Br (Black, x, t2, t3),
                        Br (Black, z, t4, t5)
                )) ->
                    Br (Black, x, 
                            Br (Black, w, t1, t2),
                            Br (Black, y,
                                    t3,
                                    Br (Red, z, t4, t5)))

        (* Default *)
        | t -> t

    let rec insert_aux x = function
        | Lf _ -> Br (Red, x, Lf Black, Lf Black)
        | Br (c, y, t1, t2) ->
                match Key.compare x y with
                | GREATER -> balance (insert_aux x t1)
                | EQUAL -> Br (c, y, t1, t2) 
                | LESS -> balance (insert_aux x t2)
                

    let insert x t =
        match insert_aux x t with
        | Lf _ -> failwith "Impossible, I'm just adding this case for an exhaustive match."
        | Br (_, x, t1, t2) -> Br (Black, x, t1, t2)


    let rec max = function
        | Lf _ -> failwith "Don't call max on a leaf!"
        | Br (_, x, _, Lf _) -> x
        | Br (_, _, _, t) -> max t


    let rec remove x t =
        match t with
        (* If x not found just return the original tree *)
        | Lf c -> Lf c

        (* Traverse tree if root is not x *)
        | Br (c, y, t1, t2) when y <> x -> (
                match Key.compare x y with
                | GREATER -> Br (c, y, t1, remove x t2)
                | EQUAL -> failwith "Impossible: case handled above."
                | LESS -> Br (c, y, remove x t1, t2)
            )

        (* Cases for nodes with no subtrees *)
        | Br (Red, _, Lf _, Lf _) -> Lf Black
        | Br (Black, _, Lf _, Lf _) -> Lf DoubleBlack

        (* Cases for node with one subtree *)
        | Br (Red, _, Lf _, Br _) 
        | Br (Red, _, Br _, Lf _)  
        | Br (Black, _, Br (Black, _, _, _), Lf _) 
        | Br (Black, _, Lf _, Br (Black, _, _, _)) -> 
                failwith "Red-Black invariant violated"

        | Br (Black, _, Br (Red, y, t1, t2), Lf _) 
        | Br (Black, _, Lf _, Br (Red, y, t1, t2)) ->
                Br (Black, y, t1, t2)

        (* Reduce case of node with two subtrees to case of node with one subtree *)
        | Br (c, _, t1, t2) -> let m = max t1 in Br (c, m, remove m t1, t2)
    ;;

    let double_black = function 
        | Br (DoubleBlack, _, _, _)
        | Lf DoubleBlack -> true
        | _ -> false
        
    (* Move DoubleBlack nodes from children to parents or eliminate entirely if
        possible. *)
    let rec bubble = function
        | Br (c, x, l, r) when double_black l || double_black r -> (
            try Br (incr c, x, tree_decr l, tree_decr r) 
            with ColorArithmeticError _ 
                -> failwith "If this got raised, RB invariant was violated."
            )
        | t -> t

    let delete _ _ = failwith "Not implemented."
end
