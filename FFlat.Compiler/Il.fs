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

    type IlRecordMember =
        {
            name : string
            type' : IlType
        }

    type IlParameter =
        | IlNamedParameter of string * IlType
        | IlUnitParameter

    type IlDeclaration =
        | IlRecord of string * IlRecordMember list
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

    let tryResolveType = function
        | "unit" -> IlUnitType
        | "int" -> IlIntType
        | x -> failwith (sprintf "Bad type: %s" x)

    let getParameterType = function
        | IlNamedParameter (_, type') -> type'
        | IlUnitParameter -> IlUnitType

    let rec exprBuildIl parameters = function
        | AstUnitLiteral -> IlUnitLiteral
        | AstIntLiteral (value) -> IlIntLiteral (value)
        | AstIdentifierLiteral (name) ->
            match
                tryFindIndexItem
                    (function
                        | IlNamedParameter (paramName, _) -> name = paramName
                        | _ -> false)
                    parameters
                with
            | Some (index, param) -> IlArgumentReference (index, getParameterType param)
            | None -> failwith (sprintf "Bad param ref: %s" name)
        | AstBinOpExpression (op, left, right) ->
            IlBinOpExpression (op, exprBuildIl parameters left, exprBuildIl parameters right)

    let rec declBuildIl = function
        | AstRecord (name, members) -> // Lol
            IlRecord
                (name,
                List.map
                    (fun x ->
                        {
                            name = fst x
                            type' = tryResolveType (snd x)
                        })
                    members)
        | AstFunction (name, parameters, expr) ->
            let ilParams =
                parameters
                |> List.map (function
                    | AstNamedParameter (name, type') -> IlNamedParameter (name, tryResolveType type')
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
        | x -> x // TODO

    let checkTypes = IlModule.mapDecls declCheckTypes
