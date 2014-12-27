module FFlat.Compiler.Il
    open FFlat.Compiler.Common
    open FFlat.Compiler.Ast

    type IlExpression =
        | IlBinOpExpression of BinOp * IlExpression * IlExpression
        | IlLiteral of int

    type IlDeclaration =
        | IlFunction of string * IlExpression

    type IlModule =
        {
            name : string
            decls : IlDeclaration list
        }

    let rec exprBuildIl = function
        | AstBinOpExpression (op, left, right) ->
            IlBinOpExpression (op, exprBuildIl left, exprBuildIl right)
        | AstLiteral (value) -> IlLiteral (value)

    let rec declBuildIl = function
        | AstFunction (name, expr) -> IlFunction (name, exprBuildIl expr)

    let moduleBuildIl (astModule : AstModule) =
        {
            name = astModule.name
            decls = List.map declBuildIl astModule.decls
        }
