module FFlat.Compiler.Codegen
    open System
    open System.Reflection
    open System.Reflection.Emit
    open FFlat.Compiler.Common
    open FFlat.Compiler.Il

    let ilTypeToMsilType = function
        | IlUnitType -> typeof<unit>
        | IlIntType -> typeof<int>
        | _ -> failwith "Aw, unknown type :("

    let ilTypeToFunctionReturnMsilType x =
        match ilTypeToMsilType x with
        | x when x = typeof<unit> -> null
        | x -> x

    let binOpToOpCode = function
        | Add -> OpCodes.Add
        | Mul -> OpCodes.Mul

    let rec exprCodegen (ilg : ILGenerator) = function
        | IlUnitLiteral -> ()
        | IlIntLiteral (value) -> ilg.Emit(OpCodes.Ldc_I4, value)

        | IlBinOpExpression (op, left, right) ->
            exprCodegen ilg left
            exprCodegen ilg right
            ilg.Emit(binOpToOpCode op)

    let declCodegen (typeBuilder : TypeBuilder) = function
        | IlFunction (name, returnType, expr) ->
            let methodBuilder =
                typeBuilder.DefineMethod(
                    name,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    ilTypeToFunctionReturnMsilType returnType,
                    [||])
            let ilg = methodBuilder.GetILGenerator()
            exprCodegen ilg expr
            ilg.Emit(OpCodes.Ret)

    let codegen ilModule =
        let assemblyName = new AssemblyName("TestAssembly")
        let appDomain = AppDomain.CurrentDomain
        let assemblyBuilder =
            appDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name)

        let typeBuilder =
            moduleBuilder.DefineType(ilModule.name,
                TypeAttributes.Public ||| TypeAttributes.Class)

        List.iter (declCodegen typeBuilder) ilModule.decls

        typeBuilder.CreateType() |> ignore

        assemblyBuilder
