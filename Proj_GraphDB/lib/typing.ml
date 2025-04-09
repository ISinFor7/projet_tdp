open Graphstruct
open Lang
open Instr
 
type environment = { types:  db_tp; bindings: (vname * label) list }

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)
  
exception FieldAccError of string
exception TypeError of string


type tc_result = (environment, string list) result

(* Functions for manipulating the environment *)

let add_var vn t (env:environment) = 
  {env with bindings = (vn,t)::env.bindings}

let remove_var vn env = 
  {env with bindings = List.remove_assoc vn env.bindings}

(* TODO: add more auxiliary functions here *)

let rec type_exist = function list_types -> function
  | [] -> true
  | (x :: xs) -> (List.mem x list_types) && (type_exist list_types xs);;

let rec no_duplicates = function
  | [] -> true
  | (x :: xs) -> not (List.mem x xs) && (no_duplicates xs);;

let types_rel_exist rtdecls ntdecls = 
  type_exist (List.map (fun (DBN(n, _)) -> n) ntdecls) (List.map (fun (DBR(n, _,_)) -> n) rtdecls) &&
  type_exist (List.map (fun (DBN(n, _)) -> n) ntdecls) (List.map (fun (DBR(_, _,n)) -> n) rtdecls) 

(* verify that types are unique (no duplicate declaration of a type) *)
let types_unique ntdecls = 
  no_duplicates (List.map (fun (DBN(n, _)) -> n) ntdecls) 

let check_graph_types (DBG (ntdecls, rtdecls)) = 
  if types_unique ntdecls
  then if types_rel_exist rtdecls ntdecls
    then Result.Ok () 
    else Result.Error "rel"
  else Result.Error "duplicates";;

(* TODO: fill in details *)
let rec tp_expr env = function
  Const v -> IntT
| AttribAcc (vn, fn) -> IntT
| BinOp (bop, e1, e2) -> tp_expr env e1

(* check expression e with expected type et in environment env *)
let check_expr e et env : tc_result = 
  try 
    if tp_expr env e = et
    then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et) ]
  with 
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]
  
let vname_not_exist list_vnames = function
(v:vname) -> not (List.mem v list_vnames)

let label_exist (list_types: vname list) (l: label) : bool = List.mem l list_types

let tc_instr (i: instruction) (env: environment) : tc_result = 
  match i with
  | IActOnNode (_act, vn, lb) -> if vname_not_exist (List.map (fun ((n, _)) -> n) env.bindings) vn then
                                  if label_exist ((fun (DBG (n, _)) ->  (List.map (fun (DBN(h,_)) -> h)) n) env.types) lb 
                                    then (match _act with
  |CreateAct -> Result.Ok (add_var vn lb env)
  |MatchAct -> Result.Ok env )
                                  else Result.Error ["type doesn't exist."] 
                                else Result.Error ["name already exist."]
  | IActOnRel (_act, vn1, lb, vn2) -> Result.Error ["not yet implemented IActOnRel."]
  | IDeleteNode (vn) -> Result.Error ["not yet implemented IDeleteNode."]
  | IDeleteRel (vn1,vn2,vn3) -> Result.Error ["not yet implemented IDeleteRel."]
  | IReturn (vnl) -> Result.Error ["not yet implemented IReturn."]
  | IWhere (expr) -> Result.Error ["not yet implemented IWhere."]
  | ISet (vn1, vn2, expr)-> Result.Error ["not yet implemented ISet."]


(* type check list of instructions and stop on error *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs


  (* TODO: typecheck instructions *)
let typecheck_instructions continue gt instrs np = 
  let r = tc_instrs_stop gt instrs in   (* call to real typechecker here *)
  match r with
  | Result.Error etc -> Printf.printf "%s\n" (String.concat "\n" etc); 
                        failwith "stopped"
  | Result.Ok env -> np
  

  (* Typecheck program; 
     If continue is true, continue typechecking even 
     when errors have been discovered (can be ignored in a first version) *)  
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                        failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np

(* Returns true if list is without duplicates *)


