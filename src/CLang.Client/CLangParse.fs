
module ClangParse

open FParsec
open Clang

type UserState = unit
type Parser<'t> = Parser<'t, UserState>



let pInt: Parser<int> = parse {
    let! head = anyOf "123456789"
    let! body = many digit
    return string head + System.String.Concat(body) |> int
}


let pIdent: Parser<string> = identifier (IdentifierOptions())

let pExpr, pExprRef = createParserForwardedToRef<CExpr, unit>()

let pFuncArgs : Parser<CExpr list> = parse {
    do! skipChar '('
    do! spaces
    let! ret = sepBy pExpr (pchar ',' >>. spaces)
    do! spaces
    do! skipChar ')'
    return ret
}



let pRawAtom: Parser<CExpr> = parse {
    let! num = opt pInt
    match num with
    | None ->
        let! ident = pIdent
        let! _ = spaces
        let! args = opt pFuncArgs
        match args with
        | None -> return Var ident
        | Some(args) -> return Apply (ident, args)
    | Some(num) ->
        return Literal num
}

let pParen: Parser<CExpr> = parse {
    do! skipChar '('
    do! spaces
    let! ret = pExpr
    do! spaces
    do! skipChar ')'
    return ret
}

let pAtom: Parser<CExpr> = pParen <|> pRawAtom

let pSignedAtom: Parser<CExpr> = parse {
    let! inv = opt <| pchar '-'
    match inv with
    | None ->
        let! ret = pAtom
        return ret
    | Some(_) ->
        let! ret = pAtom |>> Inverse
        return ret
}

let parseMultDiv: Parser<CExpr -> CExpr -> CExpr> = parse {
    do! spaces
    let! ret = anyOf "*/"
    do! spaces
    if ret = '*' then
        return fun l r -> Mult (l, r)
    else
        return fun l r -> Div (l, r)
}

let parseAddMinus: Parser<CExpr -> CExpr -> CExpr> = parse {
    do! spaces
    let! ret = anyOf "+-"
    do! spaces
    if ret = '+' then
        return fun l r -> Add (l, r)
    else
        return fun l r -> Minus (l, r)
}

let parseCompare: Parser<CExpr -> CExpr -> CExpr> = parse {
    do! spaces
    let! f = pstring "<" <|> pstring ">" <|> pstring "=" <|> pstring "!="
    do! spaces
    if f = "!=" then
        return (fun l r -> Nq (l, r))
    elif f = ">" then
        let! eq = opt <| pchar '='
        match eq with
        | None -> return (fun l r -> Gt (l, r))
        | Some(_) -> return (fun l r -> Gq (l, r))
    elif f = "=" then
        do! skipChar '='
        return (fun l r -> Eq (l, r))
    else
        let! eq = opt <| pchar '='
        match eq with
        | None -> return (fun l r -> Lt (l, r))
        | Some(_) -> return (fun l r -> Lq (l, r))
}


let pFactor = chainl1 (pSignedAtom .>> spaces) parseMultDiv
let pTerm = chainl1 (pFactor .>> spaces) parseAddMinus

do pExprRef := chainl1 (pTerm .>> spaces) parseCompare

let pStmt, pStmtRef = createParserForwardedToRef<CStmt, unit>()

let pDecl = parse {
    let! _ = skipString "int"
    do! spaces1
    let! ident = pIdent
    do! spaces
    do! skipChar ';'
    return Decl ident
}

let pAssign = parse {
    let! ident = pIdent
    do! spaces
    do! skipChar '='
    do! spaces
    let! expr = pExpr
    do! spaces
    do! skipChar ';'
    return Assign (ident, expr)
}

let pRet = parse {
    do! skipString "return"
    do! spaces
    let! expr = opt pExpr
    match expr with
    | Some(e) ->
        do! spaces
        do! skipChar ';'
        return RetVal e
    | None ->
        do! skipChar ';'
        return Ret
}

let pIf = parse {
    do! skipString "if"
    do! spaces

    do! skipChar '('
    do! spaces
    let! cond = pExpr
    do! spaces
    do! skipChar ')'

    do! spaces
    let! th = pStmt

    do! spaces
    do! skipString "else"
    do! spaces

    let! el = pStmt
    return If (cond, th, el)
}

let pWhile = parse {
    do! skipString "while"
    do! spaces
    do! skipChar '('
    do! spaces
    let! cond = pExpr
    do! spaces
    do! skipChar ')'
    do! spaces

    let! body = pStmt
    return While (cond, body)
}

let pBlock = parse {
    do! skipChar '{'
    do! spaces
    let! stmts = sepBy pStmt spaces
    do! spaces
    do! skipChar '}'
    return Block stmts
}

let pEval = parse {
    let! expr = pExpr
    do! spaces
    do! skipChar ';'
    return Eval expr
}

do pStmtRef := pBlock <|> pWhile <|> pIf <|> pRet <|> pDecl <|> attempt pAssign <|> pEval;


let pFunc = parse {
    let! ret_type = (attempt <| pstring "int") <|> pstring "void"
    do! spaces
    let! funcName = pIdent
    do! spaces
    do! skipChar '('
    do! spaces
    let! args = sepBy pIdent spaces
    do! spaces
    do! skipChar ')'
    do! spaces
    do! skipChar '{'
    do! spaces
    do! spaces
    let! stmts = sepBy pStmt spaces
    do! skipChar '}'
    return funcName, {args=args; body=stmts; hasRet=ret_type = "int"}
}

let pProgram = parse {
    do! spaces
    let! funcs = sepBy pFunc spaces
    return funcs
}