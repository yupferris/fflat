#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

open System
open System.Reflection
open System.Reflection.Emit
open FParsec

// AST + Parser
type AstDeclaration =
    | AstModule of string * AstDeclaration
    | AstFunction of string * AstExpression

and AstExpression =
    | AstAddExpression of AstExpression * AstExpression
    | AstLiteral of int

let whitespace = spaces
let equals = pstring "="
//let openBrace = pstring "{"
//let closeBrace = pstring "}"
let plus = pstring "+"
let identifier' = identifier (new IdentifierOptions())
//let type' = pstring "type"
let let' = pstring "let"
let module' = pstring "module"

let expr, (exprImpl : Parser<AstExpression, _> ref) = createParserForwardedToRef()
let integer = pint32 .>> whitespace
let intLiteral = integer |>> AstLiteral
let addExpression =
    intLiteral
    .>> plus .>> whitespace
    .>>. intLiteral
    |>> AstAddExpression
exprImpl := attempt addExpression <|> intLiteral

(*let valueDeclaration =
    let' .>> whitespace
    >>. identifier' .>> whitespace
    .>> equals .>> whitespace
    .>>. expr
    |>> Value*)

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
    |>> AstModule

let parseModule code =
    match run moduleDeclaration code with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

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

// IL
type IlDeclaration =
    | IlModule of string * IlDeclaration
    | IlFunction of string * IlExpression

and IlExpression =
    | IlLiteral of int
    | IlAdd of IlExpression * IlExpression

let rec exprBuildIl = function
    | AstLiteral (value) -> IlLiteral (value)
    | AstAddExpression (lhs, rhs) -> IlAdd(exprBuildIl lhs, exprBuildIl rhs)
    | _ -> failwith "wat"

let rec declBuildIl = function
    | AstModule (name, decl) -> IlModule (name, declBuildIl decl)
    | AstFunction (name, expr) -> IlFunction (name, exprBuildIl expr)

// Codegen
let rec exprCodegen (ilg : ILGenerator) = function
    | IlLiteral (value) -> ilg.Emit(OpCodes.Ldc_I4, value)
    | IlAdd(lhs, rhs) -> exprCodegen ilg lhs
                         exprCodegen ilg rhs
                         ilg.Emit(OpCodes.Add)

let declCodegen (typeBuilder : TypeBuilder) = function
    | IlModule _ -> failwith "well darn"
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

let codegen = function
    | IlModule (name, decl) ->
        let assemblyName = new AssemblyName("TestAssembly")
        let appDomain = AppDomain.CurrentDomain
        let assemblyBuilder =
            appDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name)

        let typeBuilder =
            moduleBuilder.DefineType(name,
                TypeAttributes.Public ||| TypeAttributes.Class)
        
        declCodegen typeBuilder decl

        typeBuilder.CreateType() |> ignore

        assemblyBuilder

    | _ -> failwith "Expected IL module declaration"

// Test
let assembly =
    //parseDeclaration "let jake = 80085 + 29"
    parseModule @"

    module FirstVertical
        let firstFunc () = 1337 + 80085

    "
    //|> declFoldConstants
    |> declBuildIl
    |> codegen


assembly.GetType("FirstVertical").GetMethod("firstFunc").Invoke(null, [||])
:?> int
