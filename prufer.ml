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

let get_leafs nodes graph =
  PairsSet.iter (fun (x,_) -> nodes := remove_elt x !nodes) graph;
  !nodes

let rec encode_rec graph =
  begin
    let list = get_leafs (ref (generate_list (PairsSet.cardinal graph+1))) graph in
    
  end


(* Graph represented as a set of tuples *)
(* List of partitions comes from the inductive path defined in the why3 file, by inductively going through each vertice and following its path  
  Maybe we can keep an HashTbl that keeps the partitions of the graph *)
let rec decode_rec prufer list graph =
  print_set graph; print_endline "\n";
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
  print_set result; print_set result2; print_set result3

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