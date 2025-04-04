(* Normalized language, after removal of patterns and other syntactic sugar *)

open Lang

type action = CreateAct | MatchAct
  [@@deriving show]

type instruction
  = IActOnNode of action * vname * label
  | IActOnRel of action * vname * label * vname
  | IDeleteNode of vname
  | IDeleteRel of vname * label * vname 
  | IReturn of vname list
  | IWhere of expr
  | ISet of vname * fieldname * expr 
  [@@deriving show]

(* Normalized query *)
type norm_query = NormQuery of instruction list
  [@@deriving show]

type norm_prog = NormProg of db_tp * norm_query
  [@@deriving show]


  let normalize_node_pattern act = function 
  | DeclPattern (v, l, attrs) -> 
      let base = IActOnNode (act, v, l) in
      let sets = List.map (fun (f, e) -> ISet(v, f, e)) attrs in
      (v, base :: sets)
  | VarRefPattern v -> (v, [])


let rec normalize_pattern act = function 
| SimpPattern p -> normalize_node_pattern act p
| CompPattern (npt, rl, pt) ->
  let x_var,x_act = (normalize_node_pattern act npt) in
  let y_var,y_act = normalize_pattern act pt in
  let action = IActOnRel (act, x_var, rl, y_var) in
  (x_var, x_act @ y_act @[action])

let normalize_clause = function
  | Create pats -> 
    List.concat_map (fun  p -> snd (normalize_pattern CreateAct p)) pats
  | Match pats -> List.concat_map (fun p -> snd (normalize_pattern MatchAct p)) pats
  | Delete (DeleteNodes vs) -> List.map (fun v -> IDeleteNode v) vs
  | Delete (DeleteRels rels) -> List.map (fun (v1, lbl, v2) -> IDeleteRel(v1, lbl, v2)) rels
  | Return vars -> [IReturn vars]
  | Where e -> [IWhere e]
  | Set assigns -> List.map (fun (v, f, e) -> ISet(v, f, e)) assigns

let normalize_query (Query cls) = NormQuery (List.concat_map normalize_clause cls)

let normalize_prog (Prog(tds, q)) =
  (* Function to extract and transform tds into the expected format *)
  let extract_tds tds =
    (* Assuming Graphstruct.db_graph has a constructor DBN that contains the relevant data *)
    match tds with
    | Graphstruct.DBN (name, attrs) -> 
        (* Convert the attributes into the form (field_name, attrib_type) *)
        (name, List.map (fun (f, t) -> (f, match t with 
            | Graphstruct.StringT -> `String
            | Graphstruct.IntT -> `Int
            | _ -> `Other)) attrs)
    | _ -> failwith "Unsupported db_graph structure"
  in
  
  (* Extract and transform the tds data *)
  let transformed_tds = [extract_tds tds] in

  (* Print the types before normalization *)
  Printf.printf "Types before normalization: %s\n" 
    (String.concat ", " (List.map (fun (label, attrs) -> 
      Printf.sprintf "(%s, [%s])" label (String.concat ", " (List.map (fun (f, t) -> Printf.sprintf "%s: %s" f (show_attrib_tp t)) attrs))
    ) transformed_tds));

  (* Normalize the query *)
  let normalized_query = normalize_query q in
  
  (* Print the normalized query *)
  Printf.printf "Normalized query: %s\n" 
    (match normalized_query with
     | NormQuery instructions -> 
         String.concat ", " (List.map (fun i -> 
           match i with
           | IActOnNode (act, v, l) -> Printf.sprintf "IActOnNode(%s, %s, %s)" (show_action act) v l
           | IActOnRel (act, v1, l, v2) -> Printf.sprintf "IActOnRel(%s, %s, %s, %s)" (show_action act) v1 l v2
           | IDeleteNode v -> Printf.sprintf "IDeleteNode(%s)" v
           | IDeleteRel (v1, l, v2) -> Printf.sprintf "IDeleteRel(%s, %s, %s)" v1 l v2
           | IReturn vs -> Printf.sprintf "IReturn(%s)" (String.concat ", " vs)
           | IWhere e -> Printf.sprintf "IWhere(%s)" (show_expr e)
           | ISet (v, f, e) -> Printf.sprintf "ISet(%s, %s, %s)" v f (show_expr e)
         ) instructions)
    );

  (* Return the normalized program *)
  NormProg(tds, normalized_query)
