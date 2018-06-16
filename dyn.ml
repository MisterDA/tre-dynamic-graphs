(* Copyright 2018 Antonin Décimo
   SPDX-License-Identifier: MIT

   Algorithm by:

   B Bui Xuan, Afonso Ferreira et Aubin Jarry. «Computing shortest,
   fastest, and foremost journeys in dynamic networks». In :
   International Journal of Foundations of Computer Science 14.02
   (2003), p. 267-285.
 *)

type time = float
type time_interval = time * time (* start * finish *)

(* We assume that if and edge e = (u, v) exists, both u and v exist. *)
type node = {
    name : string;
    node_schedule : time_interval list; (* lexicographic sort *)
    neighbours : neighbour list;        (* start + traversal <= finish *)
  }
and neighbour = {
    node : node;
    arc_schedule : time_interval list;
    traversal : time;
  }
type graph = node list

module NodeHashtbl = struct
  include Hashtbl.Make (struct
              type t = node
              let equal u v = u.name = v.name
              let hash = Hashtbl.hash
            end)

  let choose_opt tbl =
    let length = length tbl in
    if length = 0 then None
    else
      let n = Random.int length in
      let i = ref 0 in
      let choice = ref None in
      iter (fun k v -> if !i = n then choice := Some (k, v)) tbl;
      !choice
end

module TimedNodeHashtbl =
  Hashtbl.Make (struct
      type t = node * time
      let equal (u, t) (v, t') = u.name = v.name && t = t'
      let hash = Hashtbl.hash
    end)


let build_shortest_path g s =
  let st = s, 0. in

  let tree = TimedNodeHashtbl.create 16 in
  TimedNodeHashtbl.add tree st st;
  let nodes_d0 = ref [st] in          (* nodes at depth d *)
  let nodes_d1 = ref [] in            (* nodes at depth d + 1 *)

  let earliest = NodeHashtbl.create 16 in
  let location = NodeHashtbl.create 16 in
  NodeHashtbl.add earliest s 0.;
  List.iter (fun u -> if u.name <> s.name then
                        NodeHashtbl.replace earliest u infinity) g;
  let d = ref 1 in
  NodeHashtbl.add location s st;

  let not_located = NodeHashtbl.create 16 in
  List.iter (fun u ->
      if u.name <> s.name then NodeHashtbl.add not_located u ()) g;

  let inner parent child =
    let (u, t), (v, t') = parent, child in
    if NodeHashtbl.mem not_located v then
      begin
        NodeHashtbl.add location v child;
        NodeHashtbl.remove not_located v;
      end;
    if NodeHashtbl.find earliest v > t' then
      begin
        NodeHashtbl.replace earliest v t';
        TimedNodeHashtbl.replace tree child parent;
        nodes_d1 := child :: !nodes_d1
      end
  in

  let foreach_neighbours parent =
    let {neighbours}, t = parent in
    let after {node = v; arc_schedule; traversal} =
      let rec aux' = function
        | [] -> ()
        | (start, _) :: l ->
           let t' = start +. traversal in
           if t' >= t then
             let child = (v, t') in
             inner parent child
           else aux' l
      in
      aux' arc_schedule
    in
    List.iter after neighbours
  in

  let foreach_depth u =
    let rec aux = function
      | [] -> ()
      | (u', _) as tnode :: tl ->
         foreach_neighbours tnode;
         aux tl
    in
    aux !nodes_d0
  in

  let rec find_not_located () =
    match NodeHashtbl.choose_opt not_located with
    | None -> ()
    | Some (u, _) ->
       foreach_depth u;
       incr d;
       nodes_d0 := !nodes_d1;
       nodes_d1 := [];
       find_not_located ()
  in

  find_not_located ();

  tree, location


let shortest_path tree location t =
  let rec aux = function
    | [] -> assert false
    | current :: acc ->
       let parent = TimedNodeHashtbl.find tree current in
       let (u, t), (v, t') = parent, current in
       if u.name = v.name && t = t' then parent :: acc
       else aux (parent :: current :: acc)
  in
  let start = NodeHashtbl.find location t in
  aux [start]


let rec print_path = function
  | [] -> ()
  | [v, t] -> Printf.printf "%s %f\n" v.name t
  | (v, t) :: tl -> Printf.printf "%s %f -> " v.name t; print_path tl


let main () =
  let rec a = {
      name = "a";
      node_schedule = [1., 4.];
      neighbours = [
          {node = b; arc_schedule = [1., 2.]; traversal = 0.0};
          {node = c; arc_schedule = [2., 2.]; traversal = 0.0};
          {node = e; arc_schedule = [3., 3.]; traversal = 0.0}
        ];
    }
  and b = {
      name = "b";
      node_schedule = [1., 4.];
      neighbours = [
          {node = a; arc_schedule = [1., 2.]; traversal = 0.0};
          {node = e; arc_schedule = [4., 4.]; traversal = 0.0}
        ];
    }
  and c = {
      name = "c";
      node_schedule = [1., 4.];
      neighbours = [
          {node = e; arc_schedule = [1., 1.]; traversal = 0.0};
          {node = a; arc_schedule = [2., 2.]; traversal = 0.0};
          {node = e; arc_schedule = [3., 3.]; traversal = 0.0};
          {node = d; arc_schedule = [3., 4.]; traversal = 0.0}
        ];
    }
  and d = {
      name = "d";
      node_schedule = [1., 4.];
      neighbours = [
          {node = c; arc_schedule = [3., 4.]; traversal = 0.0}
        ];
    }
  and e = {
      name = "e";
      node_schedule = [1., 4.];
      neighbours = [
          {node = c; arc_schedule = [1., 1.]; traversal = 0.0};
          {node = a; arc_schedule = [3., 3.]; traversal = 0.0};
          {node = c; arc_schedule = [3., 3.]; traversal = 0.0};
          {node = b; arc_schedule = [4., 4.]; traversal = 0.0}
        ];
    } in
  let graph = [a; b; c; d; e] in

  let tree, location = build_shortest_path graph a in
  let path = shortest_path tree location d in
  print_path path


let () = main ()
