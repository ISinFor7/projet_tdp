
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHERE
    | TP of (
# 3 "lib/parser.mly"
       (Lang.attrib_tp)
# 16 "lib/parser.ml"
  )
    | SUB
    | STRINGCONSTANT of (
# 6 "lib/parser.mly"
       (string)
# 22 "lib/parser.ml"
  )
    | SET
    | RPAREN
    | RETURN
    | RBRACKET
    | RBRACE
    | NE
    | MUL
    | MOD
    | MATCH
    | LT
    | LPAREN
    | LE
    | LBRACKET
    | LBRACE
    | INTCONSTANT of (
# 5 "lib/parser.mly"
       (int)
# 41 "lib/parser.ml"
  )
    | IDENTIFIER of (
# 2 "lib/parser.mly"
       (string)
# 46 "lib/parser.ml"
  )
    | GT
    | GE
    | EQ
    | EOF
    | DOT
    | DIV
    | DELETE
    | CREATE
    | COMMA
    | COLON
    | BLOR
    | BLAND
    | BCONSTANT of (
# 4 "lib/parser.mly"
       (bool)
# 63 "lib/parser.ml"
  )
    | ARROW
    | ADD
  
end

include MenhirBasics

# 24 "lib/parser.mly"
   open Lang 
# 74 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_main) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState005 : (('s, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 005.
        Stack shape : LPAREN IDENTIFIER.
        Start symbol: main. *)

  | MenhirState012 : (('s, _menhir_box_main) _menhir_cell1_attrib_decl, _menhir_box_main) _menhir_state
    (** State 012.
        Stack shape : attrib_decl.
        Start symbol: main. *)

  | MenhirState016 : (('s, _menhir_box_main) _menhir_cell1_tpDecl, _menhir_box_main) _menhir_state
    (** State 016.
        Stack shape : tpDecl.
        Start symbol: main. *)

  | MenhirState024 : (('s, _menhir_box_main) _menhir_cell1_nodeTpRef _menhir_cell0_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 024.
        Stack shape : nodeTpRef IDENTIFIER.
        Start symbol: main. *)

  | MenhirState034 : (('s, _menhir_box_main) _menhir_cell1_list_tpDecl_, _menhir_box_main) _menhir_state
    (** State 034.
        Stack shape : list(tpDecl).
        Start symbol: main. *)

  | MenhirState035 : (('s, _menhir_box_main) _menhir_cell1_WHERE, _menhir_box_main) _menhir_state
    (** State 035.
        Stack shape : WHERE.
        Start symbol: main. *)

  | MenhirState037 : (('s, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 037.
        Stack shape : LPAREN.
        Start symbol: main. *)

  | MenhirState059 : (('s, _menhir_box_main) _menhir_cell1_expr _menhir_cell0_comp_op, _menhir_box_main) _menhir_state
    (** State 059.
        Stack shape : expr comp_op.
        Start symbol: main. *)

  | MenhirState061 : (('s, _menhir_box_main) _menhir_cell1_expr _menhir_cell0_blogic, _menhir_box_main) _menhir_state
    (** State 061.
        Stack shape : expr blogic.
        Start symbol: main. *)

  | MenhirState063 : (('s, _menhir_box_main) _menhir_cell1_expr _menhir_cell0_arith_op, _menhir_box_main) _menhir_state
    (** State 063.
        Stack shape : expr arith_op.
        Start symbol: main. *)

  | MenhirState066 : (('s, _menhir_box_main) _menhir_cell1_SET, _menhir_box_main) _menhir_state
    (** State 066.
        Stack shape : SET.
        Start symbol: main. *)

  | MenhirState070 : (('s, _menhir_box_main) _menhir_cell1_IDENTIFIER _menhir_cell0_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 070.
        Stack shape : IDENTIFIER IDENTIFIER.
        Start symbol: main. *)

  | MenhirState075 : (('s, _menhir_box_main) _menhir_cell1_assign, _menhir_box_main) _menhir_state
    (** State 075.
        Stack shape : assign.
        Start symbol: main. *)

  | MenhirState077 : (('s, _menhir_box_main) _menhir_cell1_RETURN, _menhir_box_main) _menhir_state
    (** State 077.
        Stack shape : RETURN.
        Start symbol: main. *)

  | MenhirState079 : (('s, _menhir_box_main) _menhir_cell1_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 079.
        Stack shape : IDENTIFIER.
        Start symbol: main. *)

  | MenhirState083 : (('s, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_state
    (** State 083.
        Stack shape : MATCH.
        Start symbol: main. *)

  | MenhirState090 : (('s, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER _menhir_cell0_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 090.
        Stack shape : LPAREN IDENTIFIER IDENTIFIER.
        Start symbol: main. *)

  | MenhirState092 : (('s, _menhir_box_main) _menhir_cell1_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 092.
        Stack shape : IDENTIFIER.
        Start symbol: main. *)

  | MenhirState098 : (('s, _menhir_box_main) _menhir_cell1_attr_pair, _menhir_box_main) _menhir_state
    (** State 098.
        Stack shape : attr_pair.
        Start symbol: main. *)

  | MenhirState104 : (('s, _menhir_box_main) _menhir_cell1_pattern, _menhir_box_main) _menhir_state
    (** State 104.
        Stack shape : pattern.
        Start symbol: main. *)

  | MenhirState119 : (('s, _menhir_box_main) _menhir_cell1_npattern _menhir_cell0_relspec, _menhir_box_main) _menhir_state
    (** State 119.
        Stack shape : npattern relspec.
        Start symbol: main. *)

  | MenhirState123 : (('s, _menhir_box_main) _menhir_cell1_DELETE, _menhir_box_main) _menhir_state
    (** State 123.
        Stack shape : DELETE.
        Start symbol: main. *)

  | MenhirState135 : (('s, _menhir_box_main) _menhir_cell1_CREATE, _menhir_box_main) _menhir_state
    (** State 135.
        Stack shape : CREATE.
        Start symbol: main. *)

  | MenhirState139 : (('s, _menhir_box_main) _menhir_cell1_clause, _menhir_box_main) _menhir_state
    (** State 139.
        Stack shape : clause.
        Start symbol: main. *)


and 's _menhir_cell0_arith_op = 
  | MenhirCell0_arith_op of 's * (Lang.barith)

and ('s, 'r) _menhir_cell1_assign = 
  | MenhirCell1_assign of 's * ('s, 'r) _menhir_state * (string * string * Lang.expr)

and ('s, 'r) _menhir_cell1_attr_pair = 
  | MenhirCell1_attr_pair of 's * ('s, 'r) _menhir_state * (string * Lang.expr)

and ('s, 'r) _menhir_cell1_attrib_decl = 
  | MenhirCell1_attrib_decl of 's * ('s, 'r) _menhir_state * (Lang.attrib_decl)

and 's _menhir_cell0_blogic = 
  | MenhirCell0_blogic of 's * (Lang.blogic)

and ('s, 'r) _menhir_cell1_clause = 
  | MenhirCell1_clause of 's * ('s, 'r) _menhir_state * (Lang.clause)

and 's _menhir_cell0_comp_op = 
  | MenhirCell0_comp_op of 's * (Lang.bcompar)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Lang.expr)

and ('s, 'r) _menhir_cell1_list_tpDecl_ = 
  | MenhirCell1_list_tpDecl_ of 's * ('s, 'r) _menhir_state * (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t list)

and ('s, 'r) _menhir_cell1_nodeTpRef = 
  | MenhirCell1_nodeTpRef of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_npattern = 
  | MenhirCell1_npattern of 's * ('s, 'r) _menhir_state * (Lang.node_pattern)

and ('s, 'r) _menhir_cell1_pattern = 
  | MenhirCell1_pattern of 's * ('s, 'r) _menhir_state * (Lang.pattern)

and 's _menhir_cell0_relspec = 
  | MenhirCell0_relspec of 's * (string)

and ('s, 'r) _menhir_cell1_tpDecl = 
  | MenhirCell1_tpDecl of 's * ('s, 'r) _menhir_state * (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t)

and ('s, 'r) _menhir_cell1_CREATE = 
  | MenhirCell1_CREATE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DELETE = 
  | MenhirCell1_DELETE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENTIFIER = 
  | MenhirCell1_IDENTIFIER of 's * ('s, 'r) _menhir_state * (
# 2 "lib/parser.mly"
       (string)
# 259 "lib/parser.ml"
)

and 's _menhir_cell0_IDENTIFIER = 
  | MenhirCell0_IDENTIFIER of 's * (
# 2 "lib/parser.mly"
       (string)
# 266 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MATCH = 
  | MenhirCell1_MATCH of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SET = 
  | MenhirCell1_SET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHERE = 
  | MenhirCell1_WHERE of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (Lang.prog) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 124 "lib/parser.mly"
      ( BAadd )
# 292 "lib/parser.ml"
     : (Lang.barith))

let _menhir_action_02 =
  fun () ->
    (
# 125 "lib/parser.mly"
      ( BAsub )
# 300 "lib/parser.ml"
     : (Lang.barith))

let _menhir_action_03 =
  fun () ->
    (
# 126 "lib/parser.mly"
      ( BAmul )
# 308 "lib/parser.ml"
     : (Lang.barith))

let _menhir_action_04 =
  fun () ->
    (
# 127 "lib/parser.mly"
      ( BAdiv )
# 316 "lib/parser.ml"
     : (Lang.barith))

let _menhir_action_05 =
  fun () ->
    (
# 128 "lib/parser.mly"
      ( BAmod )
# 324 "lib/parser.ml"
     : (Lang.barith))

let _menhir_action_06 =
  fun e fn vn ->
    (
# 49 "lib/parser.mly"
                                                      ( (vn, fn, e) )
# 332 "lib/parser.ml"
     : (string * string * Lang.expr))

let _menhir_action_07 =
  fun xs ->
    let fields = 
# 241 "<standard.mly>"
    ( xs )
# 340 "lib/parser.ml"
     in
    (
# 69 "lib/parser.mly"
                                                            ( fields )
# 345 "lib/parser.ml"
     : ((string * Lang.expr) list))

let _menhir_action_08 =
  fun e f ->
    (
# 72 "lib/parser.mly"
                                  ( (f, e) )
# 353 "lib/parser.ml"
     : (string * Lang.expr))

let _menhir_action_09 =
  fun i t ->
    (
# 132 "lib/parser.mly"
                                    ( (i, t) )
# 361 "lib/parser.ml"
     : (Lang.attrib_decl))

let _menhir_action_10 =
  fun xs ->
    let ads = 
# 241 "<standard.mly>"
    ( xs )
# 369 "lib/parser.ml"
     in
    (
# 134 "lib/parser.mly"
                                                           ( ads )
# 374 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_11 =
  fun () ->
    (
# 120 "lib/parser.mly"
        (BLand)
# 382 "lib/parser.ml"
     : (Lang.blogic))

let _menhir_action_12 =
  fun () ->
    (
# 121 "lib/parser.mly"
       (BLor)
# 390 "lib/parser.ml"
     : (Lang.blogic))

let _menhir_action_13 =
  fun xs ->
    let pts = 
# 241 "<standard.mly>"
    ( xs )
# 398 "lib/parser.ml"
     in
    (
# 42 "lib/parser.mly"
                                               ( Create pts )
# 403 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_14 =
  fun xs ->
    let pts = 
# 241 "<standard.mly>"
    ( xs )
# 411 "lib/parser.ml"
     in
    (
# 43 "lib/parser.mly"
                                              ( Match pts )
# 416 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_15 =
  fun pts ->
    (
# 44 "lib/parser.mly"
                               ( Delete pts )
# 424 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_16 =
  fun xs ->
    let vars = 
# 241 "<standard.mly>"
    ( xs )
# 432 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                   ( Return vars )
# 437 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_17 =
  fun cond ->
    (
# 46 "lib/parser.mly"
                     ( Where cond )
# 445 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_18 =
  fun xs ->
    let assignments = 
# 241 "<standard.mly>"
    ( xs )
# 453 "lib/parser.ml"
     in
    (
# 47 "lib/parser.mly"
                                                   ( Set assignments )
# 458 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_19 =
  fun () ->
    (
# 112 "lib/parser.mly"
     ( BCeq )
# 466 "lib/parser.ml"
     : (Lang.bcompar))

let _menhir_action_20 =
  fun () ->
    (
# 113 "lib/parser.mly"
     ( BCge )
# 474 "lib/parser.ml"
     : (Lang.bcompar))

let _menhir_action_21 =
  fun () ->
    (
# 114 "lib/parser.mly"
     ( BCgt )
# 482 "lib/parser.ml"
     : (Lang.bcompar))

let _menhir_action_22 =
  fun () ->
    (
# 115 "lib/parser.mly"
     ( BCle )
# 490 "lib/parser.ml"
     : (Lang.bcompar))

let _menhir_action_23 =
  fun () ->
    (
# 116 "lib/parser.mly"
     ( BClt )
# 498 "lib/parser.ml"
     : (Lang.bcompar))

let _menhir_action_24 =
  fun () ->
    (
# 117 "lib/parser.mly"
     ( BCne )
# 506 "lib/parser.ml"
     : (Lang.bcompar))

let _menhir_action_25 =
  fun xs ->
    let vars = 
# 241 "<standard.mly>"
    ( xs )
# 514 "lib/parser.ml"
     in
    (
# 75 "lib/parser.mly"
                                                               ( DeleteNodes vars )
# 519 "lib/parser.ml"
     : (Lang.delete_pattern))

let _menhir_action_26 =
  fun rlabel v1 v2 ->
    (
# 76 "lib/parser.mly"
                                                                                                              ( DeleteRels [(v1, rlabel, v2)] )
# 527 "lib/parser.ml"
     : (Lang.delete_pattern))

let _menhir_action_27 =
  fun a ->
    (
# 94 "lib/parser.mly"
                   (
     (*Printf.printf "Parsed primary_expr: %s\n" (string_of_expr a);*)
     a
     )
# 538 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_28 =
  fun e1 e2 op ->
    (
# 98 "lib/parser.mly"
                                     ( 
    (*Printf.printf "Parsed comparison: %s\n" (string_of_expr e1);*)
    BinOp (BCompar op, e1, e2) 
  )
# 549 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_29 =
  fun e1 e2 op ->
    (
# 102 "lib/parser.mly"
                                      ( 
    (*Printf.printf "Parsed arithmetic: %s\n" (string_of_expr e1);*)
    BinOp (BArith op, e1, e2) 
  )
# 560 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_30 =
  fun e1 e2 op ->
    (
# 106 "lib/parser.mly"
                                    ( 
    (*Printf.printf "Parsed arithmetic: %s\n" (string_of_expr e1);*)
    BinOp (BLogic op, e1, e2) 
  )
# 571 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_31 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 579 "lib/parser.ml"
     : (Lang.clause list))

let _menhir_action_32 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 587 "lib/parser.ml"
     : (Lang.clause list))

let _menhir_action_33 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 595 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t list))

let _menhir_action_34 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 605 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t list))

let _menhir_action_35 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 615 "lib/parser.ml"
     : (string list))

let _menhir_action_36 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 623 "lib/parser.ml"
     : (string list))

let _menhir_action_37 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 631 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_38 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 639 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_39 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 647 "lib/parser.ml"
     : ((string * Lang.expr) list))

let _menhir_action_40 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 655 "lib/parser.ml"
     : ((string * Lang.expr) list))

let _menhir_action_41 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 663 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_42 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 671 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_43 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 679 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_44 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 687 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_45 =
  fun _1 ->
    (
# 28 "lib/parser.mly"
               ( _1 )
# 695 "lib/parser.ml"
     : (Lang.prog))

let _menhir_action_46 =
  fun a i ->
    (
# 130 "lib/parser.mly"
                                                                        ( DBN (i, a) )
# 703 "lib/parser.ml"
     : ((string, Lang.attrib_decl list) Graphstruct.db_node))

let _menhir_action_47 =
  fun si ->
    (
# 139 "lib/parser.mly"
                                                  ( si )
# 711 "lib/parser.ml"
     : (string))

let _menhir_action_48 =
  fun t v ->
    (
# 62 "lib/parser.mly"
    ( DeclPattern(v, t, []) )
# 719 "lib/parser.ml"
     : (Lang.node_pattern))

let _menhir_action_49 =
  fun a t v ->
    (
# 64 "lib/parser.mly"
    ( DeclPattern(v, t, a) )
# 727 "lib/parser.ml"
     : (Lang.node_pattern))

let _menhir_action_50 =
  fun v ->
    (
# 66 "lib/parser.mly"
    ( VarRefPattern(v) )
# 735 "lib/parser.ml"
     : (Lang.node_pattern))

let _menhir_action_51 =
  fun np ->
    (
# 53 "lib/parser.mly"
                ( SimpPattern np )
# 743 "lib/parser.ml"
     : (Lang.pattern))

let _menhir_action_52 =
  fun np p rs ->
    (
# 54 "lib/parser.mly"
                                           ( CompPattern (np, rs, p) )
# 751 "lib/parser.ml"
     : (Lang.pattern))

let _menhir_action_53 =
  fun fn vn ->
    (
# 82 "lib/parser.mly"
     ( AttribAcc(vn, fn) )
# 759 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_54 =
  fun c ->
    (
# 84 "lib/parser.mly"
     ( Const(BoolV(c)) )
# 767 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_55 =
  fun c ->
    (
# 86 "lib/parser.mly"
     ( Const(IntV(c)) )
# 775 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_56 =
  fun c ->
    (
# 88 "lib/parser.mly"
     ( Const(StringV(c)) )
# 783 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_57 =
  fun e ->
    (
# 90 "lib/parser.mly"
     ( e )
# 791 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_58 =
  fun q td ->
    (
# 31 "lib/parser.mly"
     ( let (nts, rts) = List.partition_map Fun.id td in Prog (DBG(nts, rts), q) )
# 799 "lib/parser.ml"
     : (Lang.prog))

let _menhir_action_59 =
  fun cls ->
    (
# 38 "lib/parser.mly"
                          ( Query cls )
# 807 "lib/parser.ml"
     : (Lang.query))

let _menhir_action_60 =
  fun rlab si ti ->
    (
# 143 "lib/parser.mly"
           ( Graphstruct.DBR (si, rlab, ti) )
# 815 "lib/parser.ml"
     : ((string, string) Graphstruct.db_rel))

let _menhir_action_61 =
  fun rlabel ->
    (
# 57 "lib/parser.mly"
                                                             ( rlabel )
# 823 "lib/parser.ml"
     : (string))

let _menhir_action_62 =
  fun rlabel ->
    (
# 58 "lib/parser.mly"
                                                             ( rlabel )
# 831 "lib/parser.ml"
     : (string))

let _menhir_action_63 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 839 "lib/parser.ml"
     : (string list))

let _menhir_action_64 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 847 "lib/parser.ml"
     : (string list))

let _menhir_action_65 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 855 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_66 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 863 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_67 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 871 "lib/parser.ml"
     : ((string * Lang.expr) list))

let _menhir_action_68 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 879 "lib/parser.ml"
     : ((string * Lang.expr) list))

let _menhir_action_69 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 887 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_70 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 895 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_71 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 903 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_72 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 911 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_73 =
  fun n ->
    (
# 34 "lib/parser.mly"
                 ( Either.Left n )
# 919 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t))

let _menhir_action_74 =
  fun r ->
    (
# 35 "lib/parser.mly"
                ( Either.Right r )
# 929 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | ARROW ->
        "ARROW"
    | BCONSTANT _ ->
        "BCONSTANT"
    | BLAND ->
        "BLAND"
    | BLOR ->
        "BLOR"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | CREATE ->
        "CREATE"
    | DELETE ->
        "DELETE"
    | DIV ->
        "DIV"
    | DOT ->
        "DOT"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | GE ->
        "GE"
    | GT ->
        "GT"
    | IDENTIFIER _ ->
        "IDENTIFIER"
    | INTCONSTANT _ ->
        "INTCONSTANT"
    | LBRACE ->
        "LBRACE"
    | LBRACKET ->
        "LBRACKET"
    | LE ->
        "LE"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MATCH ->
        "MATCH"
    | MOD ->
        "MOD"
    | MUL ->
        "MUL"
    | NE ->
        "NE"
    | RBRACE ->
        "RBRACE"
    | RBRACKET ->
        "RBRACKET"
    | RETURN ->
        "RETURN"
    | RPAREN ->
        "RPAREN"
    | SET ->
        "SET"
    | STRINGCONSTANT _ ->
        "STRINGCONSTANT"
    | SUB ->
        "SUB"
    | TP _ ->
        "TP"
    | WHERE ->
        "WHERE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_138 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_list_tpDecl_ -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let cls = _v in
      let _v = _menhir_action_59 cls in
      let MenhirCell1_list_tpDecl_ (_menhir_stack, _, td) = _menhir_stack in
      let q = _v in
      let _v = _menhir_action_58 q td in
      let _1 = _v in
      let _v = _menhir_action_45 _1 in
      MenhirBox_main _v
  
  let rec _menhir_run_140 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_clause -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let MenhirCell1_clause (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_32 x xs in
      _menhir_goto_list_clause_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_clause_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState139 ->
          _menhir_run_140 _menhir_stack _v
      | MenhirState034 ->
          _menhir_run_138 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHERE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState035 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRINGCONSTANT _v ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INTCONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BCONSTANT _v ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_56 c in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primary_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let a = _v in
      let _v = _menhir_action_27 a in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState092 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RBRACE ->
          let MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, f) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_08 e f in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_attr_pair (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState098 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENTIFIER _v ->
                  _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | RBRACE ->
              let x = _v in
              let _v = _menhir_action_67 x in
              _menhir_goto_separated_nonempty_list_COMMA_attr_pair_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_02 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_arith_op : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_arith_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | STRINGCONSTANT _v_0 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState063
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | INTCONSTANT _v_1 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState063
      | IDENTIFIER _v_2 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState063
      | BCONSTANT _v_3 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState063
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRINGCONSTANT _v ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INTCONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BCONSTANT _v ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_55 c in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENTIFIER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (fn, vn) = (_v_0, _v) in
              let _v = _menhir_action_53 fn vn in
              _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_54 c in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_047 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_24 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_comp_op : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_comp_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | STRINGCONSTANT _v_0 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState059
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | INTCONSTANT _v_1 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState059
      | IDENTIFIER _v_2 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState059
      | BCONSTANT _v_3 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState059
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_03 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_049 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_23 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_22 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_052 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_21 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_053 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_20 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_054 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_19 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_055 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_04 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_056 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_blogic _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_blogic : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_blogic (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | STRINGCONSTANT _v_0 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState061
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | INTCONSTANT _v_1 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState061
      | IDENTIFIER _v_2 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState061
      | BCONSTANT _v_3 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState061
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_11 () in
      _menhir_goto_blogic _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_01 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState092 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRINGCONSTANT _v ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INTCONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BCONSTANT _v ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_attr_pair_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState090 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_attr_pair -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_attr_pair (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_68 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_attr_pair_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_094 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_40 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_attr_pair__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_attr_pair__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let xs = _v in
      let _v = _menhir_action_07 xs in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENTIFIER (_menhir_stack, t) = _menhir_stack in
          let MenhirCell0_IDENTIFIER (_menhir_stack, v) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_49 a t v in
          _menhir_goto_npattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_npattern : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_npattern (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COLON ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | IDENTIFIER _v_0 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | RBRACKET ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | ARROW ->
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              let rlabel = _v_0 in
                              let _v = _menhir_action_61 rlabel in
                              _menhir_goto_relspec _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ARROW ->
          let _menhir_stack = MenhirCell1_npattern (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COLON ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | IDENTIFIER _v_2 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | RBRACKET ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | SUB ->
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              let rlabel = _v_2 in
                              let _v = _menhir_action_62 rlabel in
                              _menhir_goto_relspec _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let np = _v in
          let _v = _menhir_action_51 np in
          _menhir_goto_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_relspec : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_npattern -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_relspec (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState119
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let v = _v in
              let _v = _menhir_action_50 v in
              _menhir_goto_npattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENTIFIER _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RPAREN ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (t, v) = (_v_0, _v) in
                      let _v = _menhir_action_48 t v in
                      _menhir_goto_npattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | LBRACE ->
                      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
                      let _menhir_stack = MenhirCell0_IDENTIFIER (_menhir_stack, _v) in
                      let _menhir_stack = MenhirCell0_IDENTIFIER (_menhir_stack, _v_0) in
                      let _menhir_s = MenhirState090 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | IDENTIFIER _v ->
                          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | RBRACE ->
                          let _v = _menhir_action_39 () in
                          _menhir_goto_loption_separated_nonempty_list_COMMA_attr_pair__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_pattern : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState119 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState135 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_120 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_npattern _menhir_cell0_relspec -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_relspec (_menhir_stack, rs) = _menhir_stack in
      let MenhirCell1_npattern (_menhir_stack, _menhir_s, np) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_52 np p rs in
      _menhir_goto_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_103 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_pattern (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState104 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let x = _v in
          let _v = _menhir_action_71 x in
          _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_pattern_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState135 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_105 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_pattern -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_pattern (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_72 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_102 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let x = _v in
      let _v = _menhir_action_44 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState135 ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState083 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_136 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_CREATE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CREATE (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_13 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_clause : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_clause (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHERE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | SET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | MATCH ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | DELETE ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | CREATE ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | EOF ->
          let _v_0 = _menhir_action_31 () in
          _menhir_run_140 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SET (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState066 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_37 () in
          _menhir_goto_loption_separated_nonempty_list_COMMA_assign__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENTIFIER _v ->
              let _menhir_stack = MenhirCell0_IDENTIFIER (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EQ ->
                  let _menhir_s = MenhirState070 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | STRINGCONSTANT _v ->
                      _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INTCONSTANT _v ->
                      _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IDENTIFIER _v ->
                      _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | BCONSTANT _v ->
                      _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_assign__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_SET -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SET (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_18 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_35 () in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE | DELETE | EOF | MATCH | RBRACKET | RETURN | SET | WHERE ->
          let x = _v in
          let _v = _menhir_action_63 x in
          _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState079 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState123 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_081 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let x = _v in
      let _v = _menhir_action_36 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState123 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState077 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_132 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_DELETE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let xs = _v in
          let _v = _menhir_action_25 xs in
          _menhir_goto_delete_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_delete_pattern : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_DELETE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_DELETE (_menhir_stack, _menhir_s) = _menhir_stack in
      let pts = _v in
      let _v = _menhir_action_15 pts in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_16 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_080 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_64 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_43 () in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_MATCH -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_14 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_122 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DELETE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENTIFIER _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COMMA ->
                  let _menhir_stack = MenhirCell1_IDENTIFIER (_menhir_stack, MenhirState123, _v) in
                  _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
              | ARROW ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | LBRACKET ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | COLON ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | IDENTIFIER _v_0 ->
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              (match (_tok : MenhirBasics.token) with
                              | RBRACKET ->
                                  let _tok = _menhir_lexer _menhir_lexbuf in
                                  (match (_tok : MenhirBasics.token) with
                                  | IDENTIFIER _v_1 ->
                                      let _tok = _menhir_lexer _menhir_lexbuf in
                                      (match (_tok : MenhirBasics.token) with
                                      | RBRACKET ->
                                          let _tok = _menhir_lexer _menhir_lexbuf in
                                          let (v2, rlabel, v1) = (_v_1, _v_0, _v) in
                                          let _v = _menhir_action_26 rlabel v1 v2 in
                                          _menhir_goto_delete_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                                      | _ ->
                                          _eRR ())
                                  | _ ->
                                      _eRR ())
                              | _ ->
                                  _eRR ())
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | RBRACKET ->
                  let _v =
                    let x = _v in
                    _menhir_action_63 x
                  in
                  _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
              | _ ->
                  _eRR ())
          | RBRACKET ->
              let _v = _menhir_action_35 () in
              _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CREATE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState135
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_43 () in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER _menhir_cell0_IDENTIFIER as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let MenhirCell0_IDENTIFIER (_menhir_stack, fn) = _menhir_stack in
          let MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, vn) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_06 e fn vn in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_assign (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState075 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENTIFIER _v ->
                  _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
              let x = _v in
              let _v = _menhir_action_65 x in
              _menhir_goto_separated_nonempty_list_COMMA_assign_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_assign_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState066 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_076 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_assign -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_assign (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_66 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_assign_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_SET -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let x = _v in
      let _v = _menhir_action_38 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_assign__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_065 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_WHERE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let MenhirCell1_WHERE (_menhir_stack, _menhir_s) = _menhir_stack in
          let cond = _v in
          let _v = _menhir_action_17 cond in
          _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr _menhir_cell0_arith_op as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | CREATE | DELETE | EOF | MATCH | RBRACE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell0_arith_op (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_29 e1 e2 op in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr _menhir_cell0_blogic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | CREATE | DELETE | EOF | MATCH | RBRACE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell0_blogic (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_30 e1 e2 op in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr _menhir_cell0_comp_op as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | CREATE | DELETE | EOF | MATCH | RBRACE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell0_comp_op (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_28 e1 e2 op in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_57 e in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  let _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_tpDecl_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHERE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | SET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | MATCH ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | DELETE ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | CREATE ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | EOF ->
          let _v_0 = _menhir_action_31 () in
          _menhir_run_138 _menhir_stack _v_0
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_tpDecl -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_tpDecl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_34 x xs in
      _menhir_goto_list_tpDecl_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_tpDecl_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState016 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENTIFIER _v ->
              let _menhir_stack = MenhirCell0_IDENTIFIER (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RPAREN ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
              | LBRACE ->
                  let _menhir_s = MenhirState005 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | IDENTIFIER _v ->
                      _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | RBRACE ->
                      let _v = _menhir_action_41 () in
                      _menhir_goto_loption_separated_nonempty_list_COMMA_attrib_decl__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_IDENTIFIER (_menhir_stack, si) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_47 si in
      _menhir_goto_nodeTpRef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_nodeTpRef : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState024 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState016 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_028 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_nodeTpRef _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_IDENTIFIER (_menhir_stack, rlab) = _menhir_stack in
      let MenhirCell1_nodeTpRef (_menhir_stack, _menhir_s, si) = _menhir_stack in
      let ti = _v in
      let _v = _menhir_action_60 rlab si ti in
      let r = _v in
      let _v = _menhir_action_74 r in
      _menhir_goto_tpDecl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_tpDecl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tpDecl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v_0 = _menhir_action_33 () in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_nodeTpRef (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COLON ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | IDENTIFIER _v ->
                      let _menhir_stack = MenhirCell0_IDENTIFIER (_menhir_stack, _v) in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | RBRACKET ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | ARROW ->
                              let _menhir_s = MenhirState024 in
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              (match (_tok : MenhirBasics.token) with
                              | LPAREN ->
                                  let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
                                  let _tok = _menhir_lexer _menhir_lexbuf in
                                  (match (_tok : MenhirBasics.token) with
                                  | COLON ->
                                      let _tok = _menhir_lexer _menhir_lexbuf in
                                      (match (_tok : MenhirBasics.token) with
                                      | IDENTIFIER _v ->
                                          let _menhir_stack = MenhirCell0_IDENTIFIER (_menhir_stack, _v) in
                                          let _tok = _menhir_lexer _menhir_lexbuf in
                                          (match (_tok : MenhirBasics.token) with
                                          | RPAREN ->
                                              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
                                          | _ ->
                                              _eRR ())
                                      | _ ->
                                          _eRR ())
                                  | _ ->
                                      _eRR ())
                              | _ ->
                                  _eRR ())
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TP _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (t, i) = (_v_0, _v) in
          let _v = _menhir_action_09 i t in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_attrib_decl (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState012 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENTIFIER _v ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | RBRACE ->
              let x = _v in
              let _v = _menhir_action_69 x in
              _menhir_goto_separated_nonempty_list_COMMA_attrib_decl_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_attrib_decl_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState012 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState005 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_013 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_attrib_decl -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_attrib_decl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_70 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_attrib_decl_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_008 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_42 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_attrib_decl__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_attrib_decl__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let xs = _v in
      let _v = _menhir_action_10 xs in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENTIFIER (_menhir_stack, i) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_46 a i in
          let n = _v in
          let _v = _menhir_action_73 n in
          _menhir_goto_tpDecl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_33 () in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 145 "lib/parser.mly"
  

# 2493 "lib/parser.ml"
