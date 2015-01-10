module FFlat.Compiler.Optimization
    open FFlat.Compiler.Common
    open FFlat.Compiler.Il

    let binOpToNativeOp = function
        | Add -> (+)
        | Mul -> (*)

    let rec exprFoldConstants = function
        | IlBinOpExpression (op, left, right) ->
            let foldedLeft = exprFoldConstants left
            let foldedRight = exprFoldConstants right
            let defaultValue = IlBinOpExpression(op, foldedLeft, foldedRight)
            match foldedLeft with
            | IlIntLiteral foldedLeftValue ->
                match foldedRight with
                | IlIntLiteral foldedRightValue ->
                    IlIntLiteral ((binOpToNativeOp op) foldedLeftValue foldedRightValue)
                | _ -> defaultValue
            | _ -> defaultValue
        | x -> x

    let declFoldConstants = function
        | IlFunction (name, parameters, returnType, expr) ->
            IlFunction (name, parameters, returnType, exprFoldConstants expr)
        | x -> x

    let foldConstants (ilModule : IlModule) =
        {
            ilModule with
                decls = List.map declFoldConstants ilModule.decls
        }
