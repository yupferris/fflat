﻿module FFlat.Compiler.Parser
    open FParsec
    open FFlat.Compiler.Common
    open FFlat.Compiler.Ast

    let whitespace = spaces
    let equals = pstring "="
    let colon = pstring ":"
    let openParen = pstring "("
    let closeParen = pstring ")"
    //let underscore = pstring "_"
    let parens x =
        openParen .>> whitespace >>. x .>> whitespace .>> closeParen .>> whitespace
    //let openBrace = pstring "{"
    //let closeBrace = pstring "}"
    let identifier' = identifier (new IdentifierOptions())
    //let type' = pstring "type"
    let let' = pstring "let"
    let module' = pstring "module"
    let begin' = pstring "begin"
    let end' = pstring "end"
    let int' = pstring "int"
    let unitValue = pstring "()"

    let opp = new OperatorPrecedenceParser<_, _, _>()
    let expr = opp.ExpressionParser
    let integer = pint32 .>> whitespace
    let unitLiteral = unitValue |>> fun _ -> AstUnitLiteral
    let intLiteral = integer |>> AstIntLiteral
    let identifierLiteral = identifier' |>> AstIdentifierLiteral
    let term =
        (unitLiteral .>> whitespace)
        <|> (intLiteral .>> whitespace)
        <|> (identifierLiteral .>> whitespace)
        <|> parens expr
    opp.TermParser <- term
    let binOp op x y = AstBinOpExpression (op, x, y)
    opp.AddOperator(InfixOperator("+", whitespace, 1, Associativity.Left, binOp Add))
    opp.AddOperator(InfixOperator("*", whitespace, 2, Associativity.Left, binOp Mul))

    let parameter =
        ((unitValue |>> fun _ -> AstUnitParameter)
        <|> (parens (identifier' .>> whitespace .>> colon .>> whitespace .>>. int') |>> AstNamedParameter))
        //<|> (underscore |>> fun _ -> AstUnnamedParameter)
        .>> whitespace

    let functionDeclaration =
        let' .>> whitespace
        >>. identifier' .>> whitespace
        .>>. many1 parameter
        .>> equals .>> whitespace
        .>>. expr
        |>> fun xyz ->
            let xy, z = xyz
            let x, y = xy
            AstFunction (x, y, z)

    let declaration = functionDeclaration

    let moduleDeclaration =
        whitespace
        .>> module' .>> whitespace
        >>. identifier' .>> whitespace
        .>> equals .>> whitespace
        .>> begin' .>> whitespace
        .>>. many functionDeclaration
        .>> whitespace
        |>> (fun (name, decls) ->
                {
                    name = name
                    decls = decls
                })
        .>> whitespace
        .>> end'

    let parseModule code =
        match run moduleDeclaration code with
        | Success (result, _, _) -> result
        | Failure (message, _, _) -> failwith message
