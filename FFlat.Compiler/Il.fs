module FFlat.Compiler.Il
    open FFlat.Compiler.Common
    open FFlat.Compiler.Ast

    type IlType =
        | IlUnknownType
        | IlUnitType
        | IlIntType

    type IlExpression =
        | IlBinOpExpression of BinOp * IlExpression * IlExpression
        | IlLiteral of int

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
        | AstBinOpExpression (op, left, right) ->
            IlBinOpExpression (op, exprBuildIl left, exprBuildIl right)
        | AstLiteral (value) -> IlLiteral (value)

    let rec declBuildIl = function
        | AstFunction (name, _, expr) -> IlFunction (name, IlUnknownType, exprBuildIl expr)

    let moduleBuildIl (astModule : AstModule) =
        {
            name = astModule.name
            decls = List.map declBuildIl astModule.decls
        }

    let rec exprGetType = function
        | IlBinOpExpression (_, _, _) -> IlIntType
        | IlLiteral _ -> IlIntType

    let rec declCheckTypes = function
        | IlFunction (name, _, expr) -> IlFunction (name, exprGetType expr, expr)

    let moduleCheckTypes = IlModule.mapDecls declCheckTypes
