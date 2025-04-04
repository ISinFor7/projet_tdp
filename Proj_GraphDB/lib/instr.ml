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

let normalize_prog (Prog(tds, q)) = NormProg(tds, normalize_query q)
