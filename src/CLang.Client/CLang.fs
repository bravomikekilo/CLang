module Clang

exception InvalidProgramException of string
exception ExecuteException of string

type CExpr = Literal of int
           | Var of string
           | Add of left: CExpr * right: CExpr
           | Minus of left: CExpr * right: CExpr
           | Inverse of op: CExpr
           | Mult of left: CExpr * right: CExpr
           | Div of left :CExpr * right: CExpr
           | Gt of left: CExpr * right: CExpr
           | Lt of left: CExpr * right: CExpr
           | Gq of left: CExpr * right: CExpr
           | Lq of left: CExpr * right: CExpr
           | Eq of left: CExpr * right: CExpr
           | Nq of left: CExpr * right: CExpr
           | Apply of name: string * args: CExpr list

type CStmt = Decl of string
           | Assign of ident: string * expr: CExpr
           | Ret
           | RetVal of CExpr
           | If of cond: CExpr * th: CStmt * el: CStmt
           | While of cond: CExpr * body: CStmt
           | Block of CStmt list
           | Eval of CExpr

type CInst = Expr of CExpr | Stmt of CStmt

type CFunc = {
    args: string list
    body: CStmt list
    hasRet: bool
}

// definition of the CProgram
type CProgram = Map<string, CFunc>

// operation number stack
type OpStack = int list

// instruction stack
type IStack = (int * CInst) list //

// calling stack frame
type CallFrame = {
    op: OpStack
    i: IStack
    name: string
}


// function calling stack
type CallStack = CallFrame list

// environment frame
type EnvFrame = {
    map: Map<string, int>
    frameName: string 
}

// environment stack
type Env = EnvFrame list

type CState = (CallStack * Env)


let rec findFunc (program: CProgram) (name: string) =
    match program.TryFind name with
    | Some(v) -> v
    | None -> raise (InvalidProgramException("invalid program, unbound function: " + name))


let rec findEnv (env: Env) (ident: string) =
    match env with
    | [] -> raise (InvalidProgramException("invalid program, unbound variable: " + ident))
    | x :: xs ->
        let {map=envMap; frameName=frameName} = x
        match envMap.TryFind ident with
        | Some(v) -> v
        | None -> findEnv xs ident

let updateEnv (env: Env) (ident: string) (v: int): Env =
    match env with
    | [] -> raise(InvalidProgramException("empty environment on update"))
    | x :: xs ->
        let {map=envMap; frameName=frameName} = x
        {x with map=envMap.Add(ident, v)} :: xs

let rec setEnv (env: Env) (ident: string) (v: int): Env =
    match env with
    | [] -> raise(InvalidProgramException("empty environment on set"))
    | x :: xs ->
        let {map=envMap; frameName=frameName} = x
        if envMap.ContainsKey ident
        then
            {x with map=envMap.Add(ident, v)} :: xs
        else
            x :: setEnv xs ident v



let zipWith f l1 l2 = List.zip l1 l2 |> List.map (fun (a, b) -> f a b)

let buildFrameName (funcName: string) (args: (string * int) list): string =
    let allArg = List.map (fun (a, v) -> sprintf "%s:%d" a v) args
    sprintf "%s (%s)" funcName (String.concat ", " allArg)

let execute (program: CProgram) (state: CState): CState =
    match state with
    | ([], env) -> ([], env)
    | (curr :: other as call, env) ->
        let {op = opstack; i = istack; name=callFrameName} = curr
        let (b, inst) :: rest = istack
        let execOp f l r msg =
            if b = 0 then
                let istack = (0, Expr r) :: (0, Expr l) :: (1, inst) :: rest
                let curr = {curr with i = istack}
                curr :: other, env
            else
                match opstack with
                | lv :: rv :: bulk ->
                    let istack = rest
                    let opstack = (f lv rv) :: bulk
                    // let curr = (opstack, istack)
                    let curr = {curr with op = opstack; i=istack}
                    curr :: other, env
                | _ -> raise (ExecuteException(msg))

        match inst with
        | Expr (Literal v) ->
            let istack = rest
            let opstack = v :: opstack
            // let curr = (opstack, istack, callFrameName)
            let curr = {curr with op = opstack; i = istack }
            (curr :: other, env)
        | Expr (Var v) ->
            let istack = rest
            let opstack = (findEnv env v) :: opstack
            // let curr = (opstack, istack)
            let curr = {curr with op = opstack; i = istack }
            (curr :: other, env)

        // arithmatic instruction
        | Expr (Add (left = l; right = r)) ->
            execOp (+) l r "missing op in Add"
        | Expr (Minus (left = l; right = r)) ->
            execOp (-) l r "missing op in Minus"
        | Expr (Mult (left = l; right = r)) ->
            execOp (*) l r "missing op in Mult"
        | Expr (Div (left = l; right = r)) ->
            execOp (/) l r "missing op in Div"
        | Expr (Gt (left = l; right = r)) ->
            execOp (fun l r -> if l > r then 1 else 0) l r "missing op in Lq"
        | Expr (Gq (left = l; right = r)) ->
            execOp (fun l r -> if l >= r then 1 else 0) l r "missing op in Lq"
        | Expr (Lt (left = l; right = r)) ->
            execOp (fun l r -> if l < r then 1 else 0) l r "missing op in Lq"
        | Expr (Lq (left = l; right = r)) ->
            execOp (fun l r -> if l <= r then 1 else 0) l r "missing op in Lq"
        | Expr (Eq (left = l; right = r)) ->
            execOp (fun l r -> if l = r then 1 else 0) l r "missing op in Eq"
        | Expr (Nq (left = l; right = r)) ->
            execOp (fun l r -> if l <> r then 1 else 0) l r "missing op in Eq"
        | Expr (Inverse (op = op)) ->
            if b = 0 then
                let istack = (0, Expr op) :: (1, inst) :: rest
                // let curr = (opstack, istack)
                let curr = {curr with i=istack}
                (curr :: other, env)
            else
                match opstack with
                | v :: bulk ->
                    let istack = rest
                    let opstack = - v :: bulk
                    // let curr = opstack, istack
                    let curr = {curr with i=istack; op=opstack}
                    curr :: other, env
                    
                | _ -> raise (ExecuteException("missing op in Div"))

        | Expr (Apply (name = name; args = args)) ->
            if b = 0 then
                let istack = List.append (List.map (fun arg -> (0, Expr arg)) args) ((1, inst) :: rest)
                // let curr = opstack, istack
                let curr = {curr with i=istack}
                curr :: other, env
            else
                let func = findFunc program name
                let actualArgs = List.zip (List.rev func.args) opstack
                let callFrameName = buildFrameName name (List.rev actualArgs)
                let newEnvMap = actualArgs |> Map.ofList
                let envName = sprintf "function scope %s" callFrameName
                let newOpstack = []
                let newIstack = List.map (fun s -> (0, Stmt s)) func.body
                let frame = {op=newOpstack; i=newIstack; name=callFrameName}
                let opstack = List.skip (List.length func.args) opstack
                // let curr = opstack, rest
                let curr = { curr with op = opstack; i = rest }
                frame :: curr :: other, {map=newEnvMap; frameName=envName} :: env
        | Stmt (Decl name) ->
            // let curr = opstack, rest
            let curr = {curr with i = rest}
            curr :: other, updateEnv env name 0
        | Stmt (Block stmts) ->
            if b = 0 then
                let istack = List.append (List.map (fun x -> 0, Stmt x) stmts) ((1, inst) :: rest)
                let envFrameName = "Block"
                let newEnvFrame = Map.empty
                let curr = {curr with i=istack}
                
                curr :: other, {map=newEnvFrame; frameName=envFrameName} :: env
            else
                let curr = {curr with i=rest}
                curr :: other, env.Tail
        | Stmt (Eval expr) ->
            if b = 0 then
                let istack = (0, Expr expr) :: (1, inst) :: rest
                let curr = {curr with i=istack}
                curr :: other, env
            else
                let opstack = []
                let curr = {curr with op=opstack; i=rest}
                curr :: other, env
        | Stmt (Assign (ident = ident; expr = expr)) ->
            if b = 0 then
                let istack = (0, Expr expr) :: (1, inst) :: rest
                let curr = {curr with i=istack}
                curr :: other, env
            else
                let v = opstack.Head
                let opstack = opstack.Tail
                let istack = rest
                // let curr = opstack, istack
                let curr = {curr with op=opstack; i=istack}
                let env = setEnv env ident v
                curr :: other, env
        | Stmt Ret ->
            other, env.Tail
        | Stmt (RetVal expr) ->
            if b = 0 then
                let istack = (0, Expr expr) :: (1, inst) :: rest
                // let curr = opstack, istack
                let curr = {curr with i=istack}
                curr :: other, env
            else
                let v = opstack.Head
                let { op=otherHeadOpstack; i=otherIstack; name=name } :: otherRest = other
                let other = {op=(v :: otherHeadOpstack); i=otherIstack; name=name} :: otherRest
                other, env
        | Stmt (If (cond = cond; th = th; el = el)) ->
            if b = 0 then
                let istack = (0, Expr cond) :: (1, inst) :: rest
                // let curr = opstack, istack
                let curr = {curr with i=istack}
                curr :: other, env
            else
                let v = opstack.Head
                let next = if v = 0 then el else th
                let istack = (0, Stmt next) :: rest
                let opstack = opstack.Tail
                // let curr = opstack, istack
                let curr = {curr with op=opstack; i=istack}
                curr :: other, env
        | Stmt (While (cond = cond; body = body)) ->
            if b = 0 then
                let istack = (0, Expr cond) :: (1, inst) :: rest
                // let curr = opstack, istack
                let curr = {curr with i=istack}
                curr :: other, env
            else
                let v = opstack.Head
                let istack = if v = 0 then rest else (0, Stmt body) :: (0, inst) :: rest
                // let curr = opstack, istack
                let curr = {curr with i=istack; op=opstack.Tail}
                curr :: other, env

    | _ -> raise (InvalidProgramException("calling stack is early empty"))








