module FFlat.Compiler.Ast
    open FFlat.Compiler.Common

    type AstExpression =
        | AstUnitLiteral
        | AstIntLiteral of int
        | AstIdentifierLiteral of string
        | AstBinOpExpression of BinOp * AstExpression * AstExpression

    type AstParameter =
        | AstNamedParameter of string * string
        //| AstUnnamedParameter
        | AstUnitParameter

    type AstDeclaration =
        | AstFunction of string * AstParameter list * AstExpression

    type AstModule =
        {
            name : string
            decls : AstDeclaration list
        }
