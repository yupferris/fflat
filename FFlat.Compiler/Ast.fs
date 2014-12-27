module FFlat.Compiler.Ast
    open FFlat.Compiler.Common

    type AstExpression =
        | AstBinOpExpression of BinOp * AstExpression * AstExpression
        | AstLiteral of int

    type AstDeclaration =
        | AstFunction of string * AstExpression

    type AstModule =
        {
            name : string
            decls : AstDeclaration list
        }
