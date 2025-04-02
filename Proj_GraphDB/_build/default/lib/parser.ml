
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

  | MenhirState045 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 045.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState047 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 047.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState049 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 049.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState051 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 051.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState054 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 054.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState056 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 056.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState058 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 058.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState060 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 060.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState062 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 062.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState064 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 064.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState066 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 066.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState068 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 068.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState070 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 070.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState073 : (('s, _menhir_box_main) _menhir_cell1_SET, _menhir_box_main) _menhir_state
    (** State 073.
        Stack shape : SET.
        Start symbol: main. *)

  | MenhirState077 : (('s, _menhir_box_main) _menhir_cell1_IDENTIFIER _menhir_cell0_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 077.
        Stack shape : IDENTIFIER IDENTIFIER.
        Start symbol: main. *)

  | MenhirState082 : (('s, _menhir_box_main) _menhir_cell1_assign, _menhir_box_main) _menhir_state
    (** State 082.
        Stack shape : assign.
        Start symbol: main. *)

  | MenhirState084 : (('s, _menhir_box_main) _menhir_cell1_RETURN, _menhir_box_main) _menhir_state
    (** State 084.
        Stack shape : RETURN.
        Start symbol: main. *)

  | MenhirState086 : (('s, _menhir_box_main) _menhir_cell1_IDENTIFIER, _menhir_box_main) _menhir_state
    (** State 086.
        Stack shape : IDENTIFIER.
        Start symbol: main. *)

  | MenhirState090 : (('s, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_state
    (** State 090.
        Stack shape : MATCH.
        Start symbol: main. *)

  | MenhirState099 : (('s, _menhir_box_main) _menhir_cell1_pattern, _menhir_box_main) _menhir_state
    (** State 099.
        Stack shape : pattern.
        Start symbol: main. *)

  | MenhirState114 : (('s, _menhir_box_main) _menhir_cell1_npattern _menhir_cell0_relspec, _menhir_box_main) _menhir_state
    (** State 114.
        Stack shape : npattern relspec.
        Start symbol: main. *)

  | MenhirState118 : (('s, _menhir_box_main) _menhir_cell1_DELETE, _menhir_box_main) _menhir_state
    (** State 118.
        Stack shape : DELETE.
        Start symbol: main. *)

  | MenhirState130 : (('s, _menhir_box_main) _menhir_cell1_CREATE, _menhir_box_main) _menhir_state
    (** State 130.
        Stack shape : CREATE.
        Start symbol: main. *)

  | MenhirState134 : (('s, _menhir_box_main) _menhir_cell1_clause, _menhir_box_main) _menhir_state
    (** State 134.
        Stack shape : clause.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_assign = 
  | MenhirCell1_assign of 's * ('s, 'r) _menhir_state * (string * string * Lang.expr)

and ('s, 'r) _menhir_cell1_attrib_decl = 
  | MenhirCell1_attrib_decl of 's * ('s, 'r) _menhir_state * (Lang.attrib_decl)

and ('s, 'r) _menhir_cell1_clause = 
  | MenhirCell1_clause of 's * ('s, 'r) _menhir_state * (Lang.clause)

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
# 282 "lib/parser.ml"
)

and 's _menhir_cell0_IDENTIFIER = 
  | MenhirCell0_IDENTIFIER of 's * (
# 2 "lib/parser.mly"
       (string)
# 289 "lib/parser.ml"
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
  fun e fn vn ->
    (
# 49 "lib/parser.mly"
                                                      ( (vn, fn, e) )
# 315 "lib/parser.ml"
     : (string * string * Lang.expr))

let _menhir_action_02 =
  fun i t ->
    (
# 102 "lib/parser.mly"
                                    ( (i, t) )
# 323 "lib/parser.ml"
     : (Lang.attrib_decl))

let _menhir_action_03 =
  fun xs ->
    let ads = 
# 241 "<standard.mly>"
    ( xs )
# 331 "lib/parser.ml"
     in
    (
# 104 "lib/parser.mly"
                                                           ( ads )
# 336 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_04 =
  fun xs ->
    let pts = 
# 241 "<standard.mly>"
    ( xs )
# 344 "lib/parser.ml"
     in
    (
# 42 "lib/parser.mly"
                                               ( Create pts )
# 349 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_05 =
  fun xs ->
    let pts = 
# 241 "<standard.mly>"
    ( xs )
# 357 "lib/parser.ml"
     in
    (
# 43 "lib/parser.mly"
                                              ( Match pts )
# 362 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_06 =
  fun pts ->
    (
# 44 "lib/parser.mly"
                               ( Delete pts )
# 370 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_07 =
  fun xs ->
    let vars = 
# 241 "<standard.mly>"
    ( xs )
# 378 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                   ( Return vars )
# 383 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_08 =
  fun cond ->
    (
# 46 "lib/parser.mly"
                     ( Where cond )
# 391 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_09 =
  fun xs ->
    let assignments = 
# 241 "<standard.mly>"
    ( xs )
# 399 "lib/parser.ml"
     in
    (
# 47 "lib/parser.mly"
                                                   ( Set assignments )
# 404 "lib/parser.ml"
     : (Lang.clause))

let _menhir_action_10 =
  fun xs ->
    let vars = 
# 241 "<standard.mly>"
    ( xs )
# 412 "lib/parser.ml"
     in
    (
# 65 "lib/parser.mly"
                                                               ( DeleteNodes vars )
# 417 "lib/parser.ml"
     : (Lang.delete_pattern))

let _menhir_action_11 =
  fun rlabel v1 v2 ->
    (
# 66 "lib/parser.mly"
                                                                                                              ( DeleteRels [(v1, rlabel, v2)] )
# 425 "lib/parser.ml"
     : (Lang.delete_pattern))

let _menhir_action_12 =
  fun a ->
    (
# 84 "lib/parser.mly"
                   ( a )
# 433 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_13 =
  fun e1 e2 ->
    (
# 85 "lib/parser.mly"
                           ( BinOp (BCompar BCeq, e1, e2) )
# 441 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_14 =
  fun e1 e2 ->
    (
# 86 "lib/parser.mly"
                           ( BinOp (BCompar BCge, e1, e2) )
# 449 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_15 =
  fun e1 e2 ->
    (
# 87 "lib/parser.mly"
                           ( BinOp (BCompar BCgt, e1, e2) )
# 457 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_16 =
  fun e1 e2 ->
    (
# 88 "lib/parser.mly"
                           ( BinOp (BCompar BCle, e1, e2) )
# 465 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_17 =
  fun e1 e2 ->
    (
# 89 "lib/parser.mly"
                           ( BinOp (BCompar BClt, e1, e2) )
# 473 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_18 =
  fun e1 e2 ->
    (
# 90 "lib/parser.mly"
                           ( BinOp (BCompar BCne, e1, e2) )
# 481 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_19 =
  fun e1 e2 ->
    (
# 91 "lib/parser.mly"
                            ( BinOp (BArith BAadd, e1, e2) )
# 489 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_20 =
  fun e1 e2 ->
    (
# 92 "lib/parser.mly"
                            ( BinOp (BArith BAsub, e1, e2) )
# 497 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_21 =
  fun e1 e2 ->
    (
# 93 "lib/parser.mly"
                            ( BinOp (BArith BAmul, e1, e2) )
# 505 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_22 =
  fun e1 e2 ->
    (
# 94 "lib/parser.mly"
                            ( BinOp (BArith BAdiv, e1, e2) )
# 513 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_23 =
  fun e1 e2 ->
    (
# 95 "lib/parser.mly"
                            ( BinOp (BArith BAmod, e1, e2) )
# 521 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_24 =
  fun e1 e2 ->
    (
# 96 "lib/parser.mly"
                              ( BinOp (BLogic BLand, e1, e2) )
# 529 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_25 =
  fun e1 e2 ->
    (
# 97 "lib/parser.mly"
                             ( BinOp (BLogic BLor, e1, e2) )
# 537 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_26 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 545 "lib/parser.ml"
     : (Lang.clause list))

let _menhir_action_27 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 553 "lib/parser.ml"
     : (Lang.clause list))

let _menhir_action_28 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 561 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t list))

let _menhir_action_29 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 571 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t list))

let _menhir_action_30 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 581 "lib/parser.ml"
     : (string list))

let _menhir_action_31 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 589 "lib/parser.ml"
     : (string list))

let _menhir_action_32 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 597 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_33 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 605 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_34 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 613 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_35 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 621 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_36 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 629 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_37 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 637 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_38 =
  fun _1 ->
    (
# 28 "lib/parser.mly"
               ( _1 )
# 645 "lib/parser.ml"
     : (Lang.prog))

let _menhir_action_39 =
  fun a i ->
    (
# 100 "lib/parser.mly"
                                                                        ( DBN (i, a) )
# 653 "lib/parser.ml"
     : ((string, Lang.attrib_decl list) Graphstruct.db_node))

let _menhir_action_40 =
  fun si ->
    (
# 109 "lib/parser.mly"
                                                  ( si )
# 661 "lib/parser.ml"
     : (string))

let _menhir_action_41 =
  fun t v ->
    (
# 61 "lib/parser.mly"
                                                        ( DeclPattern(v, t) )
# 669 "lib/parser.ml"
     : (Lang.node_pattern))

let _menhir_action_42 =
  fun v ->
    (
# 62 "lib/parser.mly"
                                 ( VarRefPattern(v) )
# 677 "lib/parser.ml"
     : (Lang.node_pattern))

let _menhir_action_43 =
  fun np ->
    (
# 53 "lib/parser.mly"
                ( SimpPattern np )
# 685 "lib/parser.ml"
     : (Lang.pattern))

let _menhir_action_44 =
  fun np p rs ->
    (
# 54 "lib/parser.mly"
                                           ( CompPattern (np, rs, p) )
# 693 "lib/parser.ml"
     : (Lang.pattern))

let _menhir_action_45 =
  fun fn vn ->
    (
# 72 "lib/parser.mly"
     ( AttribAcc(vn, fn) )
# 701 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_46 =
  fun c ->
    (
# 74 "lib/parser.mly"
     ( Const(BoolV(c)) )
# 709 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_47 =
  fun c ->
    (
# 76 "lib/parser.mly"
     ( Const(IntV(c)) )
# 717 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_48 =
  fun c ->
    (
# 78 "lib/parser.mly"
     ( Const(StringV(c)) )
# 725 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_49 =
  fun e ->
    (
# 80 "lib/parser.mly"
     ( e )
# 733 "lib/parser.ml"
     : (Lang.expr))

let _menhir_action_50 =
  fun q td ->
    (
# 31 "lib/parser.mly"
     ( let (nts, rts) = List.partition_map Fun.id td in Prog (DBG(nts, rts), q) )
# 741 "lib/parser.ml"
     : (Lang.prog))

let _menhir_action_51 =
  fun cls ->
    (
# 38 "lib/parser.mly"
                          ( Query cls )
# 749 "lib/parser.ml"
     : (Lang.query))

let _menhir_action_52 =
  fun rlab si ti ->
    (
# 113 "lib/parser.mly"
           ( Graphstruct.DBR (si, rlab, ti) )
# 757 "lib/parser.ml"
     : ((string, string) Graphstruct.db_rel))

let _menhir_action_53 =
  fun rlabel ->
    (
# 57 "lib/parser.mly"
                                                             ( rlabel )
# 765 "lib/parser.ml"
     : (string))

let _menhir_action_54 =
  fun rlabel ->
    (
# 58 "lib/parser.mly"
                                                             ( rlabel )
# 773 "lib/parser.ml"
     : (string))

let _menhir_action_55 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 781 "lib/parser.ml"
     : (string list))

let _menhir_action_56 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 789 "lib/parser.ml"
     : (string list))

let _menhir_action_57 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 797 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_58 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 805 "lib/parser.ml"
     : ((string * string * Lang.expr) list))

let _menhir_action_59 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 813 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_60 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 821 "lib/parser.ml"
     : (Lang.attrib_decl list))

let _menhir_action_61 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 829 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_62 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 837 "lib/parser.ml"
     : (Lang.pattern list))

let _menhir_action_63 =
  fun n ->
    (
# 34 "lib/parser.mly"
                 ( Either.Left n )
# 845 "lib/parser.ml"
     : (((string, Lang.attrib_decl list) Graphstruct.db_node,
   (string, string) Graphstruct.db_rel)
  Either.t))

let _menhir_action_64 =
  fun r ->
    (
# 35 "lib/parser.mly"
                ( Either.Right r )
# 855 "lib/parser.ml"
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
  
  let _menhir_run_133 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_list_tpDecl_ -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let cls = _v in
      let _v = _menhir_action_51 cls in
      let MenhirCell1_list_tpDecl_ (_menhir_stack, _, td) = _menhir_stack in
      let q = _v in
      let _v = _menhir_action_50 q td in
      let _1 = _v in
      let _v = _menhir_action_38 _1 in
      MenhirBox_main _v
  
  let rec _menhir_run_135 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_clause -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let MenhirCell1_clause (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_27 x xs in
      _menhir_goto_list_clause_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_clause_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _v
      | MenhirState034 ->
          _menhir_run_133 _menhir_stack _v
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
      let _v = _menhir_action_48 c in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primary_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let a = _v in
      let _v = _menhir_action_12 a in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState045 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_078 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER _menhir_cell0_IDENTIFIER as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let MenhirCell0_IDENTIFIER (_menhir_stack, fn) = _menhir_stack in
          let MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, vn) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_01 e fn vn in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_assign (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState082 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENTIFIER _v ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
              let x = _v in
              let _v = _menhir_action_57 x in
              _menhir_goto_separated_nonempty_list_COMMA_assign_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState045 in
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
      let _v = _menhir_action_47 c in
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
              let _v = _menhir_action_45 fn vn in
              _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_46 c in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState054 in
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
  
  and _menhir_run_047 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState047 in
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
  
  and _menhir_run_049 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState049 in
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
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState058 in
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
  
  and _menhir_run_060 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState060 in
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
  
  and _menhir_run_062 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState062 in
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
  
  and _menhir_run_064 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState064 in
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
  
  and _menhir_run_066 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState066 in
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
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState051 in
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
  
  and _menhir_run_068 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState068 in
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
  
  and _menhir_run_070 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState070 in
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
  
  and _menhir_run_056 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState056 in
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
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
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
                  let _menhir_s = MenhirState077 in
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
  
  and _menhir_goto_separated_nonempty_list_COMMA_assign_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState073 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_083 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_assign -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_assign (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_58 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_assign_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_SET -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let x = _v in
      let _v = _menhir_action_33 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_assign__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_assign__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_SET -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SET (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_09 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_clause : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_clause (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHERE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
      | SET ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
      | RETURN ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
      | MATCH ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
      | DELETE ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
      | CREATE ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
      | EOF ->
          let _v_0 = _menhir_action_26 () in
          _menhir_run_135 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SET (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState073 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_32 () in
          _menhir_goto_loption_separated_nonempty_list_COMMA_assign__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState084
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_30 () in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE | DELETE | EOF | MATCH | RBRACKET | RETURN | SET | WHERE ->
          let x = _v in
          let _v = _menhir_action_55 x in
          _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState118 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_088 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let x = _v in
      let _v = _menhir_action_31 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState118 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_127 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_DELETE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let xs = _v in
          let _v = _menhir_action_10 xs in
          _menhir_goto_delete_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_delete_pattern : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_DELETE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_DELETE (_menhir_stack, _menhir_s) = _menhir_stack in
      let pts = _v in
      let _v = _menhir_action_06 pts in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_089 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_07 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_087 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENTIFIER -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_IDENTIFIER (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_56 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_090 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_36 () in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let v = _v in
              let _v = _menhir_action_42 v in
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
                      let _v = _menhir_action_41 t v in
                      _menhir_goto_npattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
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
                              let _v = _menhir_action_53 rlabel in
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
                              let _v = _menhir_action_54 rlabel in
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
          let _v = _menhir_action_43 np in
          _menhir_goto_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_relspec : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_npattern -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_relspec (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | _ ->
          _eRR ()
  
  and _menhir_goto_pattern : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState130 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_npattern _menhir_cell0_relspec -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_relspec (_menhir_stack, rs) = _menhir_stack in
      let MenhirCell1_npattern (_menhir_stack, _menhir_s, np) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_44 np p rs in
      _menhir_goto_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_pattern (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState099 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let x = _v in
          let _v = _menhir_action_61 x in
          _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_pattern_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState130 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_100 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_pattern -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_pattern (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_62 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_097 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let x = _v in
      let _v = _menhir_action_37 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_pattern__ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState130 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_131 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_CREATE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CREATE (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_04 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_116 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_MATCH -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_05 xs in
      _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_117 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
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
                  let _menhir_stack = MenhirCell1_IDENTIFIER (_menhir_stack, MenhirState118, _v) in
                  _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
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
                                          let _v = _menhir_action_11 rlabel v1 v2 in
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
                    _menhir_action_55 x
                  in
                  _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState118 _tok
              | _ ->
                  _eRR ())
          | RBRACKET ->
              let _v = _menhir_action_30 () in
              _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CREATE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v = _menhir_action_36 () in
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_WHERE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let MenhirCell1_WHERE (_menhir_stack, _menhir_s) = _menhir_stack in
          let cond = _v in
          let _v = _menhir_action_08 cond in
          _menhir_goto_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | MATCH | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_24 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR | COMMA | CREATE | DELETE | EOF | MATCH | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_25 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_13 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_14 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_15 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_16 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_17 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | SUB | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_18 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_22 e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_23 e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_048 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_21 e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_046 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | BLAND | BLOR | COMMA | CREATE | DELETE | EOF | EQ | GE | GT | LE | LT | MATCH | NE | RETURN | RPAREN | SET | SUB | WHERE ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_20 e1 e2 in
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
          let _v = _menhir_action_49 e in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BLAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  let _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_tpDecl_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHERE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | SET ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | RETURN ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | MATCH ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | DELETE ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | CREATE ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | EOF ->
          let _v_0 = _menhir_action_26 () in
          _menhir_run_133 _menhir_stack _v_0
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_tpDecl -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_tpDecl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_29 x xs in
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
                      let _v = _menhir_action_34 () in
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
      let _v = _menhir_action_40 si in
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
      let _v = _menhir_action_52 rlab si ti in
      let r = _v in
      let _v = _menhir_action_64 r in
      _menhir_goto_tpDecl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_tpDecl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tpDecl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState016
      | CREATE | DELETE | EOF | MATCH | RETURN | SET | WHERE ->
          let _v_0 = _menhir_action_28 () in
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
          let _v = _menhir_action_02 i t in
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
              let _v = _menhir_action_59 x in
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
      let _v = _menhir_action_60 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_attrib_decl_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_008 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_35 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_attrib_decl__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_attrib_decl__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN _menhir_cell0_IDENTIFIER -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let xs = _v in
      let _v = _menhir_action_03 xs in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENTIFIER (_menhir_stack, i) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_39 a i in
          let n = _v in
          let _v = _menhir_action_63 n in
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
          let _v = _menhir_action_28 () in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 115 "lib/parser.mly"
  

# 2554 "lib/parser.ml"
