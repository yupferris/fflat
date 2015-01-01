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
        | IlBinOpExpression of BinOp * IlExpression * IlExpression

    type IlDeclaration =
        | IlFunction of string * IlType * IlExpression

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

    let rec exprBuildIl = function
        | AstUnitLiteral -> IlUnitLiteral
        | AstIntLiteral (value) -> IlIntLiteral (value)
        | AstBinOpExpression (op, left, right) ->
            IlBinOpExpression (op, exprBuildIl left, exprBuildIl right)

    let rec declBuildIl = function
        | AstFunction (name, _, expr) -> IlFunction (name, IlUnknownType, exprBuildIl expr)

    let moduleBuildIl (astModule : AstModule) =
        {
            name = astModule.name
            decls = List.map declBuildIl astModule.decls
        }

    let rec exprGetType = function
        | IlUnitLiteral -> IlUnitType
        | IlIntLiteral _ -> IlIntType
        | IlBinOpExpression (_, _, _) -> IlIntType

    let rec declCheckTypes = function
        | IlFunction (name, _, expr) -> IlFunction (name, exprGetType expr, expr)

    let moduleCheckTypes = IlModule.mapDecls declCheckTypes
