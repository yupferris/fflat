module FFlat.Compiler.Ast
    open FFlat.Compiler.Common

    type AstExpression =
        | AstBinOpExpression of BinOp * AstExpression * AstExpression
        | AstLiteral of int

    type AstParameter =
        | AstNamedParameter of string
        | AstUnnamedParameter
        | AstUnitParameter

    type AstDeclaration =
        | AstFunction of string * AstParameter list * AstExpression

    type AstModule =
        {
            name : string
            decls : AstDeclaration list
        }
