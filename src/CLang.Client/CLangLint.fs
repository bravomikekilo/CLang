module ClangLint

open Clang
open Either

type CExprType = | Int | Void

type LintError = | UnboundVar of string
                 | ReDeclVar of string
                 | FunctionRedef of string
                 | UnDefinedFunc of string
                 | WrongFuncArity of funcName: string * expect: int * actual: int
                 | VoidFuncRetValue of funcName: string
                 | FuncRetNoValue of funcName: string
                 | FuncNoRet of funcName: string
                 | InvalidMainFunc
                 | TypeError

let isVoid t =
    match t with
    | Int -> false
    | Void -> true

let flattenStmt (stmt: CStmt): CStmt list =
    match stmt with
    | Block stmts -> stmts
    | x -> [x]

let rec lintRet (funcName: string) (isVoid: bool) (body: CStmt list): bool * LintError list =
    match body with
    | [] -> false, []
    | head :: rest ->
        match head with
        | Decl _ -> lintRet funcName isVoid rest
        | Assign _ -> lintRet funcName isVoid rest
        | Eval _ -> lintRet funcName isVoid rest
        | Ret ->
            let _, lintRest = lintRet funcName isVoid rest
            if isVoid then
                true, lintRest
            else
                true, FuncRetNoValue funcName :: lintRest

        | RetVal _ ->
            let _, lintRest = lintRet funcName isVoid rest
            if isVoid then
                true, VoidFuncRetValue funcName :: lintRest
            else
                true, lintRest

        | If (cond=_; th=th; el=el) ->
            let termRest, lintRest = lintRet funcName isVoid rest
            let termTh, lintTh = lintRet funcName isVoid (flattenStmt th)
            let termEl, lintEl = lintRet funcName isVoid (flattenStmt el)
            let term = termTh && termEl
            let allLint = List.concat <| lintTh :: lintEl :: [lintRest]
            if term then
                true, allLint
            else
                termRest, allLint

        | While (cond=_; body=body) ->
            let termRest, lintRest = lintRet funcName isVoid rest
            let termBody, lintBody = lintRet funcName isVoid (flattenStmt body)
            termRest, List.append lintBody lintRest

        | Block stmts ->
            let termRest, lintRest = lintRet funcName isVoid rest
            let termBody, lintBody = lintRet funcName isVoid stmts
            let allLint = List.append lintBody lintRest
            if termBody then
                true, allLint
            else
                termRest, allLint

let lintFuncRet (func: string * CFunc): LintError list =
    let funcName, func = func
    let term, errors = lintRet funcName (not func.hasRet) func.body
    if term then
        errors
    else
        FuncNoRet funcName :: errors


let rec inferExprType (funcRet: Map<string, CExprType>) (expr: CExpr): CExprType =
    match expr with
    | Apply (name=name; args=args) ->
        let funcType = funcRet.TryFind name
        match funcType with
        | None -> Int
        | Some(x) -> x
    | _ -> Int

let rec lintExpr (funcRet: Map<string, CExprType>) (funcArity: Map<string, int>) (vars: Set<string>) (expr: CExpr): LintError list =
    let lintOp l r =
        let lintLeft = lintExpr funcRet funcArity vars l
        let leftType = inferExprType funcRet l
        let lintRight = lintExpr funcRet funcArity vars r
        let rightType = inferExprType funcRet l
        let allLint = List.append lintLeft lintRight
        if leftType = Int && rightType = Int then
            allLint
        else
            TypeError :: allLint

    match expr with
    | Literal _ -> []
    | Var ident -> if vars.Contains ident then [] else [UnboundVar ident]
    | Add (left=l; right=r) -> lintOp l r
    | Minus (left=l; right=r) -> lintOp l r
    | Mult (left=l; right=r) -> lintOp l r
    | Div (left=l; right=r) -> lintOp l r
    | Gt (left=l; right=r) -> lintOp l r
    | Lt (left=l; right=r) -> lintOp l r
    | Gq (left=l; right=r) -> lintOp l r
    | Lq (left=l; right=r) -> lintOp l r
    | Eq (left=l; right=r) -> lintOp l r
    | Nq (left=l; right=r) -> lintOp l r
    | Inverse (op=op) ->
        let lintOp = lintExpr funcRet funcArity vars op
        let opType = inferExprType funcRet op
        if opType = Void then
            TypeError :: lintOp
        else
            lintOp
    | Apply (name=name; args=args) ->
        let allTypes = List.map (fun arg -> inferExprType funcRet arg) args
        let hasTypeError = List.exists isVoid allTypes
        let allLint = List.collect (fun arg -> lintExpr funcRet funcArity vars arg) args
        let allLint = if funcArity.ContainsKey(name) then allLint else UnDefinedFunc name :: allLint
        let allLint = if hasTypeError then allLint else TypeError :: allLint
        let actualArity = List.length args
        let expectArity = match funcArity.TryFind(name) with
                          | None -> actualArity
                          | Some(x) -> x
        if expectArity <> actualArity then
            WrongFuncArity (name, expectArity, actualArity) :: allLint
        else
            allLint

let rec lintStmts funcRet funcArity (vars: Set<string>) (stmts: CStmt list): LintError list =
    match stmts with
    | [] -> []
    | stmt :: rest ->
        match stmt with
        | Decl var ->
            if vars.Contains var then
                ReDeclVar var :: lintStmts funcRet funcArity vars rest
            else
                lintStmts funcRet funcArity (vars.Add var) rest
        | Assign (ident=var; expr=expr) ->
            let exprErrors = lintExpr funcRet funcArity vars expr
            let allLint = List.append exprErrors <| lintStmts funcRet funcArity vars rest
            if vars.Contains var then
                allLint
            else
                UnboundVar var :: allLint
        | Ret -> lintStmts funcRet funcArity vars rest
        | RetVal expr ->
            let lintRest = lintStmts funcRet funcArity vars rest
            let exprLint = lintExpr funcRet funcArity vars expr
            List.append exprLint lintRest
        | If (cond=cond; th=th; el=el) ->
            let lintCond = lintExpr funcRet funcArity vars cond
            let lintTh = lintStmts funcRet funcArity vars (flattenStmt th)
            let lintEl = lintStmts funcRet funcArity vars (flattenStmt el)
            let lintRest = lintStmts funcRet funcArity vars rest
            List.concat [lintCond; lintTh; lintEl; lintRest]
        | While (cond=cond; body=body) ->
            let lintCond = lintExpr funcRet funcArity vars cond
            let lintBody = lintStmts funcRet funcArity vars (flattenStmt body)
            let lintRest = lintStmts funcRet funcArity vars rest
            List.concat [lintCond; lintBody; lintRest]
        | Block stmts ->
            let lintBody = lintStmts funcRet funcArity vars stmts
            let lintRest = lintStmts funcRet funcArity vars rest
            List.append lintBody lintRest
        | Eval expr ->
            let exprError = lintExpr funcRet funcArity vars expr
            let lintRest = lintStmts funcRet funcArity vars rest
            List.append exprError lintRest

let rec duplicate (l: 'a list) (known: Set<'a>) (ret: 'a list) =
    match l with
    | [] -> ret
    | x :: xs ->
        if known.Contains x then
            duplicate xs known (x :: ret)
        else
            duplicate xs (known.Add x) ret

let findDuplicate l = duplicate l Set.empty []


let lintFuncs (funcPairs: (string * CFunc) list) =
    let funcNames, funcs = List.unzip funcPairs
    let duplicateFuncName = findDuplicate funcNames
    match duplicateFuncName with
    | (_ :: _) as ds -> List.map FunctionRedef ds
    | [] ->
        let funcMap = Map.ofList funcPairs
        let funcArity = Map.map (fun _ func -> List.length func.args) funcMap
        let funcRet = Map.map (fun _ func -> if func.hasRet then Int else Void) funcMap
        let normalErrs = List.collect (fun func -> lintStmts funcRet funcArity (Set.ofList func.args) func.body) funcs
        let retErrs = List.collect lintFuncRet funcPairs
        List.append retErrs normalErrs

let lintProgram (funcPairs: (string * CFunc) list) =
    let simpleErrs = lintFuncs funcPairs
    let program = Map.ofList funcPairs
    if not <| program.ContainsKey "main" then
         Right <| InvalidMainFunc :: simpleErrs
    else
        let mainFunc = program.Item("main")
        if mainFunc.hasRet then
            Right <| InvalidMainFunc :: simpleErrs
        else
            match simpleErrs with
            | [] -> Left program
            | _ -> Right simpleErrs
       
