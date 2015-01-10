module FFlat.Compiler.Parser
    open FParsec
    open FFlat.Compiler.Common
    open FFlat.Compiler.Ast

    let whitespace = spaces
    let equals = pstring "="
    let colon = pstring ":"
    let semicolon = pstring ";"
    let openParen = pstring "("
    let closeParen = pstring ")"
    let openBrace = pstring "{"
    let closeBrace = pstring "}"
    //let underscore = pstring "_"
    //let betweenWhitespace left right x = left .>> whitespace >>. x .>> whitespace .>> right .>> whitespace
    let parens x = openParen .>> whitespace >>. x .>> whitespace .>> closeParen .>> whitespace//betweenWhitespace openParen closeParen
    let braces x = openBrace .>> whitespace >>. x .>> whitespace .>> closeBrace .>> whitespace//betweenWhitespace openBrace closeBrace
    let identifier' = identifier (new IdentifierOptions())
    let let' = pstring "let"
    let module' = pstring "module"
    let begin' = pstring "begin"
    let end' = pstring "end"
    let type' = pstring "type"
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
        <|> (parens (identifier' .>> whitespace .>> colon .>> whitespace .>>. int') |>> AstNamedParameter)) // TODO
        //<|> (underscore |>> fun _ -> AstUnnamedParameter)
        .>> whitespace

    let recordDeclaration =
        type' .>> whitespace
        .>> identifier' .>> whitespace
        .>> equals .>> whitespace
        .>>.
            (braces
                (sepBy1
                    (identifier' .>> whitespace .>> colon .>> whitespace .>>. int')
                    (semicolon .>> whitespace))) // TODO
        |>> AstRecord

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

    let declaration = recordDeclaration <|> functionDeclaration

    let moduleDeclaration =
        whitespace
        .>> module' .>> whitespace
        >>. identifier' .>> whitespace
        .>> equals .>> whitespace
        .>> begin' .>> whitespace
        .>>. many declaration
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
