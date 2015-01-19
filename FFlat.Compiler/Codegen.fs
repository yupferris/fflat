module FFlat.Compiler.Codegen
    open System
    open System.IO
    open System.Reflection
    open System.Reflection.Emit
    open FFlat.Compiler.Common
    open FFlat.Compiler.Il

    type CodeGenOptions =
        | RunOnly
        | Save of string

    let ilTypeToMsilType = function
        | IlUnitType -> typeof<unit>
        | IlIntType -> typeof<int>
        | _ -> failwith "Aw, unknown type :("

    let ilTypeToFunctionReturnMsilType x =
        match ilTypeToMsilType x with
        | x when x = typeof<unit> -> null
        | x -> x

    let ilParameterToMsilType = function
        | IlNamedParameter (_, type') -> ilTypeToMsilType type'
        | _ -> typeof<unit>

    let binOpToOpCode = function
        | Add -> OpCodes.Add
        | Mul -> OpCodes.Mul

    let rec exprCodegen (ilg : ILGenerator) = function
        | IlUnitLiteral -> ()
        | IlIntLiteral (value) -> ilg.Emit(OpCodes.Ldc_I4, value)
        | IlArgumentReference (index, _) ->
            match index with
            | 0 -> ilg.Emit(OpCodes.Ldarg_0)
            | 1 -> ilg.Emit(OpCodes.Ldarg_1)
            | 2 -> ilg.Emit(OpCodes.Ldarg_2)
            | 3 -> ilg.Emit(OpCodes.Ldarg_3)
            | x -> ilg.Emit(OpCodes.Ldarg, x)
        | IlBinOpExpression (op, left, right) ->
            exprCodegen ilg left
            exprCodegen ilg right
            ilg.Emit(binOpToOpCode op)

    let declCodegen (moduleBuilder : ModuleBuilder) (typeBuilder : TypeBuilder) = function
        | IlRecord (name, members) ->
            let recordTypeBuilder = moduleBuilder.DefineType(name, TypeAttributes.Public)
            members
            |> List.iter (fun x ->
                recordTypeBuilder.DefineField(
                    x.name, ilTypeToMsilType x.type', FieldAttributes.Public)
                |> ignore)
            recordTypeBuilder.CreateType() |> ignore
        | IlFunction (name, parameters, returnType, expr) ->
            let methodBuilder =
                typeBuilder.DefineMethod(
                    name,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    ilTypeToFunctionReturnMsilType returnType,
                    if parameters.Length = 1 && List.forall (function
                        | IlUnitParameter -> true
                        | _ -> false) parameters then [||]
                    else List.map ilParameterToMsilType parameters |> Array.ofList)
            let ilg = methodBuilder.GetILGenerator()
            exprCodegen ilg expr
            ilg.Emit(OpCodes.Ret)

    let codegen options ilModule =
        let appDomain = AppDomain.CurrentDomain
        let assemblyBuilder =
            match options with
            | RunOnly ->
                let assemblyName = new AssemblyName("TestAssembly")
                appDomain.DefineDynamicAssembly(
                    assemblyName,
                    AssemblyBuilderAccess.Run)
            | Save (fileName) ->
                let assemblyName =
                    new AssemblyName(
                        Path.GetFileNameWithoutExtension fileName)
                appDomain.DefineDynamicAssembly(
                    assemblyName,
                    AssemblyBuilderAccess.RunAndSave,
                    (Path.GetDirectoryName fileName))
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(ilModule.name)

        let typeBuilder =
            moduleBuilder.DefineType(ilModule.name,
                TypeAttributes.Public ||| TypeAttributes.Class)

        List.iter (declCodegen moduleBuilder typeBuilder) ilModule.decls

        typeBuilder.CreateType() |> ignore

        match options with
        | Save (fileName) -> assemblyBuilder.Save (Path.GetFileName fileName)
        | _ -> ()

        assemblyBuilder
