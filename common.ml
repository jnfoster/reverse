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
