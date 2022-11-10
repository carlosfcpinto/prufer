(*let L be the set of labels not appearing in C (leaves of T)
  for i = 1 to n - 1 do
    u = C[i]
    let v be te smallest label in L
    add_edge (u,v) to tree T
    delete v from L
    if (C[i] is the rightmost occurence of u in C) then 
      add u to the label set L 
      
  Coming from an article, not neccessarily the way to do this *)

  (*Create an empty graph of  n+2 nodes
  Choose the smallest node in graph that is not in the sequence 
  Add the edge between that node and the first intenger in sequence
  When only two vertices remain join those and we have that graph *)


module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
        | c -> c
    end

module PairsSet = Set.Make(IntPairs)

let remove_elt x list =
  (List.filter (fun z -> z <> x) list)
let smallest prufer list =
  let list_aux = (List.filter (fun x -> not (List.mem x prufer)) list) in
  List.hd list_aux (* The list is already ordered from the precondition of the function decode *)

let pp_int_pair (x,y) =
  Printf.printf "(%d,%d)\n" x y

let print_set set =
  print_endline "\n";
  PairsSet.iter (fun x -> pp_int_pair x) set


let rec generate_list n =
    let rec aux l result =
        if l > 0 then
            aux (l - 1) (l :: result)
        else
            result
    in
    aux n []

let get_leafs (nodes: int list) graph =
  let nodes_aux = ref nodes in
  PairsSet.iter (fun (x,_) -> try nodes_aux := remove_elt x !nodes_aux with Not_found -> ()) graph;
  !nodes_aux

let rec encode_rec graph nodes i=
  begin
    List.iter (fun x -> print_int x) nodes; print_newline ();
    print_int (List.length nodes); print_newline ();
    if List.length nodes <= 2 then [] else
    let list_nodes = get_leafs nodes graph in
    let (node, leaf) = List.hd (PairsSet.elements ( PairsSet.filter (fun (x,y) -> y = List.hd list_nodes || x = List.hd list_nodes) graph)) in
    print_endline "wtf3"; print_int i; print_newline ();
    let graph = PairsSet.remove (node, leaf) graph in 
    let nodes = remove_elt leaf nodes in 
    node :: encode_rec graph nodes (i+1)
    (* search through the set for node which has (a, b) where b is the lowest leaf in list
      after that, we do this recursively, by eliminating the node b and (a, b) from the set
      and calling function encode_rec with this new set and the list of leaf node must be recalculated 
      we should keep list of nodes present in graph since we are always generating it we might add a node which has already been deleted
      The function must have one more argument, the actual nodes that are in the graph, which at the start is precisely 
      generate_list (   PairsSet.cardinal graph  +  1   )*)
  end


(* Graph represented as a set of tuples *)
(* List of partitions comes from the inductive path defined in the why3 file, by inductively going through each vertice and following its path  
  Maybe we can keep an HashTbl that keeps the partitions of the graph *)
let rec decode_rec prufer list graph =
  (*print_set graph; print_endline "\n";*)
  match prufer with
    |hd::tl -> let a = smallest prufer list in
              decode_rec tl (remove_elt a list) (PairsSet.add (hd, a) graph)
    |[] -> match list with
            | hd::shd::[] -> PairsSet.add (hd, shd) graph
            |_ -> assert false


let decode_imp prufer list graph =
  for i = 0 to (List.length prufer) do
    let a = smallest prufer !list in
    graph := (PairsSet.add (List.nth prufer i, a) !graph);
    list := remove_elt a !list
  done;
  match !list with
    | hd::shd::[] -> PairsSet.add (hd, shd) !graph
    |_ -> assert false

let () = 
  let deo = [9;7;7;3;10;4;4;7;1] in
  let a = [1;7;5;7;7;1] in
  let optimal = [2;4;0;1;3;3] in
  let result3 = decode_rec optimal (generate_list (2 + List.length optimal)) PairsSet.empty in
  let result2 = decode_rec deo (generate_list (2 + List.length deo)) PairsSet.empty in
  let result = decode_rec a (generate_list (2+List.length a)) PairsSet.empty in
  let sequence = encode_rec result (generate_list ((PairsSet.cardinal result) +1)) 0 in
  let sequence2 = encode_rec result2 (generate_list ((PairsSet.cardinal result2) +1)) 0 in
  let sequence3 = encode_rec result3 (generate_list ((PairsSet.cardinal result3) +1)) 0 in
  print_set result; print_set result2; print_set result3; List.iter (fun x -> print_int x) sequence; print_newline ();
  List.iter (fun x -> print_int x) sequence2; print_newline (); List.iter (fun x -> print_int x) sequence3; print_newline ();

(* One way to find leafs is to check what nodes have no connection in the set
  So, there is no tuple (a,_) means that a is necessarily a leaf 
  From then on, the encoding algorithm comes easily, either iteratively or recursively
  Probably can finish this until tomorrow*)

(*let test_greeting_teacher () =
  Alcotest.(check string)
    "same string" "Hey Professor!"
    ("Hello Director.")

let test_greeting_director () =
  Alcotest.(check string)
    "same string" "Hello Director."
    ("Hello Director.")
    
  let () =
  Alcotest.run "Example Alcotest suite"
    [
      ( "Greetings",
        [
          Alcotest.test_case "Greeting teacher" `Quick
            test_greeting_teacher;
          Alcotest.test_case "Greeting director" `Quick
            test_greeting_director;
        ] );
    ]
*)