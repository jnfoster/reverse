(* some utilities *)


(***************************************************
 * Monads
 * **************************************************)

module type MONAD = sig
    type 'a t
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    val return: 'a -> 'a t
end

module Make (M: MONAD) = struct
    include M
    let (>>) a b = a >>= (fun _ -> b)
end

module type LOG = sig
    type t
    val empty: t
    val concat: t -> t -> t
end

module Writer_Monad (L: LOG) = struct
    
    include Make(struct 
        type 'a t = 'a * L.t
        let (>>=) (v, l) f =
            let (v', l') = f v in
            (v', L.concat l l')
        let return v = (v, L.empty)
        end)
end

module type STATE = sig
    type t
end

module State_Monad (S:STATE) = struct
    type state = S.t

    include Make(struct
        type 'a t = state -> ('a * state)
        let (>>=) m f = 
            fun s -> 
                let (x, s') = m s in f x s'
        let return v = fun s -> (v, s)
    end)
end

(*****************************************************
 *  * HashSet -- like in Java
 *   *****************************************************)
module type HashSet = sig
    type 'a t
    val make : unit -> 'a t
    val add : 'a t -> 'a -> unit
    val remove : 'a t -> 'a -> unit
    val mem : 'a t -> 'a -> bool
    val size : 'a t -> int
    val values : 'a t -> 'a list
end

module HashSet : HashSet = struct
    type 'a t = ('a, 'a) Hashtbl.t
    let make() : 'a t = Hashtbl.create 16
    let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
    let add (h : 'a t) (x : 'a) =
        if mem h x then () else Hashtbl.add h x x
        let remove (h : 'a t) (x : 'a) =
            while Hashtbl.mem h x do
                Hashtbl.remove h x
            done
            let size (h : 'a t) : int = Hashtbl.length h
            let values (h : 'a t) : 'a list =
                Hashtbl.fold (fun x y v -> y :: v) h []
end


(*****************************************************
 * Manipulating lists
 *****************************************************)

(** Given a list of size n, returns a pair of lists, one of 
 * size i and other other of size n - i; None if there is no such split *)
let split_list lst i = 
  if List.length lst < i then None
  else 
    let split = 
      List.fold_left (fun (l1, l2) (x, pos) ->
        if pos <= i then (l1@[x], l2)
        else (l1, l2@[x])) ([], []) (List.mapi (fun i x -> (x, i+1)) lst) in
    Some split

(** Extracts the i-th element of the list (indexing starts at 1) 
  * and places it at the head of the list *)
let roll_list lst i =
    match split_list lst (i-1) with 
    | Some (l1, elem::l2) -> elem::(l1@l2)
    | _ -> failwith "invalid roll"

(** Takes the first element of the list and places it at the i-th position *)
let unroll_list lst i = 
      match split_list lst i with
      | Some (elem::l1, l2) -> l1 @ [elem] @ l2
      | _ -> failwith "invalid un-roll"
  
