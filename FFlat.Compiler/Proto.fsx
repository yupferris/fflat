#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

open System
open System.Reflection
open System.Reflection.Emit
open FParsec

// AST + Parser
type AstDeclaration =
    | AstFunction of string * AstExpression

and AstExpression =
    | AstAddExpression of AstExpression * AstExpression
    | AstLiteral of int

type AstModule =
    {
        name : string
        decl : AstDeclaration
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
opp.AddOperator(
    InfixOperator("+", whitespace, 1, Associativity.Left,
        fun x y -> AstAddExpression (x, y)))

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
    .>>. functionDeclaration
    .>> whitespace
    |>> (fun (name, decl) ->
            {
                name = name
                decl = decl
            })
    .>> whitespace
    .>> end'

let parseModule code =
    match run moduleDeclaration code with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

// IL
type IlDeclaration =
    | IlFunction of string * IlExpression

and IlExpression =
    | IlAddExpression of IlExpression * IlExpression
    | IlLiteral of int

type IlModule =
    {
        name : string
        decl : IlDeclaration
    }

let rec exprBuildIl = function
    | AstAddExpression (left, right) ->
        IlAddExpression (exprBuildIl left, exprBuildIl right)
    | AstLiteral (value) -> IlLiteral (value)

let rec declBuildIl = function
    | AstFunction (name, expr) -> IlFunction (name, exprBuildIl expr)

let moduleBuildIl (astModule : AstModule) =
    {
        name = astModule.name
        decl = declBuildIl astModule.decl
    }

// Optimization
(*let rec exprFoldConstants = function
    | AstAddExpression (left, right) ->
        let foldedLeft = exprFoldConstants left
        let foldedRight = exprFoldConstants right
        let defaultValue = AstAddExpression(foldedLeft, foldedRight)
        match foldedLeft with
        | AstLiteral foldedLeftValue ->
            match foldedRight with
            | AstLiteral foldedRightValue ->
                AstLiteral (foldedLeftValue + foldedRightValue)
            | _ -> defaultValue
        | _ -> defaultValue
    | x -> x

let declFoldConstants = function
    | Value (name, expr) -> Value (name, exprFoldConstants expr)
    | x -> x*)

// Codegen
let rec exprCodegen (ilg : ILGenerator) = function
    | IlAddExpression (left, right) ->
        exprCodegen ilg left
        exprCodegen ilg right
        ilg.Emit(OpCodes.Add)

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

    declCodegen typeBuilder ilModule.decl

    typeBuilder.CreateType() |> ignore

    assemblyBuilder

// Test
let print x =
    printfn "%A" x
    x

let assembly =
    parseModule @"

    module FirstVertical
        let firstFunc () = 1337 + (80085 + 5)
    end

    "
    |> moduleBuildIl
    |> print
    |> codegen


assembly.GetType("FirstVertical").GetMethod("firstFunc").Invoke(null, [||])
:?> int
