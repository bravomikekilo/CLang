module ClangRender

open Clang
open Bolero.Html

let treeul args = ul [attr.``class`` "tree"] args
let treeli args = li [attr.``class`` "tree"] args
let subli args = li [attr.``class`` "sub"] args

let leafli args = li [attr.``class`` "leaf"] args

let cInstName (inst: CInst): string =
    match inst with
    | Expr expr ->
      match expr with
      | Literal x -> "Literal " + string x
      | Var x -> "Var " + x
      | Minus _ -> "Minus"
      | Add _ -> "Add"
      | Mult _ -> "Mult"
      | Div _ -> "Div"
      | Inverse _ -> "Inv"
      | Gt _ -> "Greater"
      | Gq _ -> "GreaterEq"
      | Lt _ -> "Less"
      | Lq _ -> "LessEq"
      | Eq _ -> "Equal"
      | Nq _ -> "NotEq"
      | Apply _ -> "Apply"
    | Stmt stmt ->
      match stmt with
      | Decl x -> "Decl " + x
      | Assign (ident=ident; expr=expr) -> "Assign " + ident
      | Ret -> "Ret"
      | RetVal _ -> "RetVal"
      | If _ -> "If"
      | While _ -> "While"
      | Block _ -> "Block"
      | Eval _ -> "Eval"

let rec renderCExpr (expr: CExpr) =
    let renderOp name l r = treeul [
        treeli [text name]
        subli [renderCExpr l]
        subli [renderCExpr r]
    ]

    match expr with
    | Literal x -> div [] [text <| string x]
    | Var x -> div [] [text x]
    | Inverse (op=op) -> treeul [
        treeli [text "Inv"]
        subli [renderCExpr op]
      ]
    | Add(left=l; right=r) -> renderOp "Add" l r
    | Minus(left=l; right=r) -> renderOp "Minus" l r
    | Mult(left=l; right=r) -> renderOp "Mult" l r
    | Div(left=l; right=r) -> renderOp "Div" l r
    | Gt(left=l; right=r) -> renderOp ">" l r
    | Gq(left=l; right=r) -> renderOp ">=" l r
    | Lt(left=l; right=r) -> renderOp ">" l r
    | Lq(left=l; right=r) -> renderOp "<=" l r
    | Eq(left=l; right=r) -> renderOp "==" l r
    | Nq(left=l; right=r) -> renderOp "!=" l r
    | Apply(name=name; args=args) ->
        let renderArg arg = subli [renderCExpr arg]
        treeul <| treeli [text <| "apply " + name] :: (List.map renderArg args)

let rec renderCStmt (stmt: CStmt) =

    match stmt with
    | Decl name -> div [] [text <| "Decl: " + name ]
    | Assign (ident=ident; expr=expr) -> treeul [
        treeli [text <| "Assign " + ident]
        subli [renderCExpr expr]
      ]
    | Ret -> div [] [text "Ret"]
    | RetVal expr -> treeul [
        treeli [text "RetVal"]
        subli [renderCExpr expr]
      ]
    | If (cond=cond; th=th; el=el) -> treeul [
        treeli [text "If"]
        subli [renderCExpr cond]
        subli [renderCStmt th]
        subli [renderCStmt el]
      ]
    | While (cond=cond; body=body) -> treeul [
        treeli [text "While"]
        subli [renderCExpr cond]
        subli [renderCStmt body]
      ]
    | Block stmts -> treeul [
        treeli [text "Block"]
        treeul <| List.map (fun stmt -> subli [renderCStmt stmt]) stmts
      ]
    | Eval expr -> treeul [
        treeli [text "Eval"]
        subli [renderCExpr expr]
      ]

let renderCInst inst =
    match inst with
    | Expr expr -> renderCExpr expr
    | Stmt stmt -> renderCStmt stmt
