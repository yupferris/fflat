#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

open System
open System.Reflection
open System.Reflection.Emit
open FParsec

// AST + Parser
type AstBinOp =
    | Add
    | Mul

type AstExpression =
    | AstBinOpExpression of AstBinOp * AstExpression * AstExpression
    | AstLiteral of int

type AstDeclaration =
    | AstFunction of string * AstExpression

type AstModule =
    {
        name : string
        decls : AstDeclaration list
    }

let whitespace = spaces
let equals = pstring "="
let openParen = pstring "("
let closeParen = pstring ")"
let parens x =
    openParen .>> whitespace >>. x .>> whitespace .>> closeParen .>> whitespace
//let openBrace = pstring "{"
//let closeBrace = pstring "}"
let identifier' = identifier (new IdentifierOptions())
//let type' = pstring "type"
let let' = pstring "let"
let module' = pstring "module"
let end' = pstring "end"

let opp = new OperatorPrecedenceParser<_, _, _>()
let expr = opp.ExpressionParser
let integer = pint32 .>> whitespace
let intLiteral = integer |>> AstLiteral
let term = parens expr <|> (intLiteral .>> whitespace)
opp.TermParser <- term
let binOp op x y = AstBinOpExpression (op, x, y)
opp.AddOperator(InfixOperator("+", whitespace, 1, Associativity.Left, binOp Add))
opp.AddOperator(InfixOperator("*", whitespace, 2, Associativity.Left, binOp Mul))

let functionDeclaration =
    let' .>> whitespace
    >>. identifier' .>> whitespace
    .>> pstring "()" .>> whitespace
    .>> equals .>> whitespace
    .>>. expr
    |>> AstFunction

let declaration = functionDeclaration

let moduleDeclaration =
    whitespace
    .>> module' .>> whitespace
    >>. identifier' .>> whitespace
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

// IL
type IlBinOp =
    | Add
    | Mul

type IlExpression =
    | IlBinOpExpression of IlBinOp * IlExpression * IlExpression
    | IlLiteral of int

type IlDeclaration =
    | IlFunction of string * IlExpression

type IlModule =
    {
        name : string
        decls : IlDeclaration list
    }

let astBinOpToIlBinOp = function
    | AstBinOp.Add -> Add
    | AstBinOp.Mul -> Mul

let rec exprBuildIl = function
    | AstBinOpExpression (op, left, right) ->
        IlBinOpExpression (astBinOpToIlBinOp op, exprBuildIl left, exprBuildIl right)
    | AstLiteral (value) -> IlLiteral (value)

let rec declBuildIl = function
    | AstFunction (name, expr) -> IlFunction (name, exprBuildIl expr)

let moduleBuildIl (astModule : AstModule) =
    {
        name = astModule.name
        decls = List.map declBuildIl astModule.decls
    }

// Optimization
let binOpToNativeOp = function
    | Add -> (+)
    | Mul -> (*)

let rec exprFoldConstants = function
    | IlBinOpExpression (op, left, right) ->
        let foldedLeft = exprFoldConstants left
        let foldedRight = exprFoldConstants right
        let defaultValue = IlBinOpExpression(op, foldedLeft, foldedRight)
        match foldedLeft with
        | IlLiteral foldedLeftValue ->
            match foldedRight with
            | IlLiteral foldedRightValue ->
                IlLiteral ((binOpToNativeOp op) foldedLeftValue foldedRightValue)
            | _ -> defaultValue
        | _ -> defaultValue
    | x -> x

let declFoldConstants = function
    | IlFunction (name, expr) -> IlFunction (name, exprFoldConstants expr)

let foldConstants (ilModule : IlModule) =
    {
        ilModule with
            decls = List.map declFoldConstants ilModule.decls
    }

// Codegen
let binOpToOpCode = function
    | Add -> OpCodes.Add
    | Mul -> OpCodes.Mul

let rec exprCodegen (ilg : ILGenerator) = function
    | IlBinOpExpression (op, left, right) ->
        exprCodegen ilg left
        exprCodegen ilg right
        ilg.Emit(binOpToOpCode op)

    | IlLiteral (value) -> ilg.Emit(OpCodes.Ldc_I4, value)

let declCodegen (typeBuilder : TypeBuilder) = function
    | IlFunction (name, expr) ->
        let methodBuilder =
            typeBuilder.DefineMethod(
                name,
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<int>,
                [||])
        let ilg = methodBuilder.GetILGenerator()
        exprCodegen ilg expr
        ilg.Emit(OpCodes.Ret)

let codegen ilModule =
    let assemblyName = new AssemblyName("TestAssembly")
    let appDomain = AppDomain.CurrentDomain
    let assemblyBuilder =
        appDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name)

    let typeBuilder =
        moduleBuilder.DefineType(ilModule.name,
            TypeAttributes.Public ||| TypeAttributes.Class)

    List.iter (declCodegen typeBuilder) ilModule.decls

    typeBuilder.CreateType() |> ignore

    assemblyBuilder

// Test
let print x =
    printfn "%A" x
    x

let assembly =
    parseModule @"

    module FirstVertical
        let firstFunc () = 2 + 3 * 4
        let secondFunc () = 2 * 3 + 4
    end

    "
    |> moduleBuildIl
    |> print
    |> foldConstants
    |> print
    |> codegen

["firstFunc"; "secondFunc"]
|> List.map (fun x -> assembly.GetType("FirstVertical").GetMethod(x).Invoke(null, [||]))
