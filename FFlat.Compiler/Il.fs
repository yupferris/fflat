module FFlat.Compiler.Il
    open FFlat.Compiler.Common
    open FFlat.Compiler.Ast

    type IlType =
        | IlUnknownType
        | IlUnitType
        | IlIntType

    type IlExpression =
        | IlUnitLiteral
        | IlIntLiteral of int
        | IlArgumentReference of int * IlType
        | IlBinOpExpression of BinOp * IlExpression * IlExpression

    type IlParameter =
        | IlNamedParameter of string * IlType
        | IlUnitParameter

    type IlDeclaration =
        | IlFunction of string * IlParameter list * IlType * IlExpression

    type IlModule =
        {
            name : string
            decls : IlDeclaration list
        }

        with
            static member mapDecls f x =
                {
                    name = x.name
                    decls = List.map f x.decls
                }

    let rec exprBuildIl parameters = function
        | AstUnitLiteral -> IlUnitLiteral
        | AstIntLiteral (value) -> IlIntLiteral (value)
        | AstIdentifierLiteral (name) ->
            IlArgumentReference
                (List.findIndex
                    (function
                        | IlNamedParameter (paramName, _) -> name = paramName
                        | _ -> false)
                    parameters,
                IlIntType)
        | AstBinOpExpression (op, left, right) ->
            IlBinOpExpression (op, exprBuildIl parameters left, exprBuildIl parameters right)

    let rec declBuildIl = function
        | AstFunction (name, parameters, expr) ->
            let ilParams =
                parameters
                |> List.map (function
                    | AstNamedParameter (name, _) -> IlNamedParameter (name, IlIntType)
                    | AstUnitParameter -> IlUnitParameter)
            IlFunction (name, ilParams, IlUnknownType, exprBuildIl ilParams expr)

    let moduleBuildIl (astModule : AstModule) =
        {
            name = astModule.name
            decls = List.map declBuildIl astModule.decls
        }

    let rec exprGetType = function
        | IlUnitLiteral -> IlUnitType
        | IlIntLiteral _ -> IlIntType
        | IlArgumentReference (_, type') -> type'
        | IlBinOpExpression (_, _, _) -> IlIntType

    let rec declCheckTypes = function
        | IlFunction (name, parameters, _, expr) ->
            IlFunction (name, parameters, exprGetType expr, expr)

    let checkTypes = IlModule.mapDecls declCheckTypes
