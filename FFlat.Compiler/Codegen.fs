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

    let emitLoadArg (ilg : ILGenerator) = function
        | 0 -> ilg.Emit(OpCodes.Ldarg_0)
        | 1 -> ilg.Emit(OpCodes.Ldarg_1)
        | 2 -> ilg.Emit(OpCodes.Ldarg_2)
        | 3 -> ilg.Emit(OpCodes.Ldarg_3)
        | x -> ilg.Emit(OpCodes.Ldarg, x)

    let rec exprCodegen (ilg : ILGenerator) = function
        | IlUnitLiteral -> ()
        | IlIntLiteral (value) -> ilg.Emit(OpCodes.Ldc_I4, value)
        | IlArgumentReference (index, _) -> emitLoadArg ilg index
        | IlBinOpExpression (op, left, right) ->
            exprCodegen ilg left
            exprCodegen ilg right
            ilg.Emit(binOpToOpCode op)

    let declCodegen (typeBuilder : TypeBuilder) = function
        | IlRecord (name, members) ->
            let recordTypeBuilder =
                typeBuilder.DefineNestedType(
                    name,
                    TypeAttributes.NestedPublic)

            // Generate backing fields
            let fieldBuilders =
                members
                |> List.map (fun x ->
                    (x,
                        recordTypeBuilder.DefineField(
                            "_" + x.name,
                            ilTypeToMsilType x.type',
                            FieldAttributes.Private)))
                |> Map.ofList

            // Generate getter properties for backing fields
            fieldBuilders
            |> Map.iter (fun member' fieldBuilder ->
                let getterType = fieldBuilder.FieldType
                let getterPropBuilder =
                    recordTypeBuilder.DefineProperty(
                        member'.name,
                        PropertyAttributes.None,
                        getterType,
                        null)
                let getter =
                    recordTypeBuilder.DefineMethod(
                        "get_" + member'.name,
                        MethodAttributes.Public |||
                        MethodAttributes.SpecialName |||
                        MethodAttributes.HideBySig,
                        getterType,
                        Type.EmptyTypes)
                let ilg = getter.GetILGenerator()
                ilg.Emit(OpCodes.Ldfld, fieldBuilder)
                ilg.Emit(OpCodes.Ret)
                getterPropBuilder.SetGetMethod(getter))

            // Generate constructor
            let ctor =
                recordTypeBuilder.DefineConstructor(
                    MethodAttributes.Public,
                    CallingConventions.Standard,
                    members
                    |> List.map (fun x -> (Map.find x fieldBuilders).FieldType)
                    |> Array.ofList)
            let ilg = ctor.GetILGenerator()
            ilg.Emit(OpCodes.Ldarg_0)
            let objCtor = typeof<obj>.GetConstructor(Type.EmptyTypes)
            ilg.Emit(OpCodes.Call, objCtor)
            members
            |> List.iteri (fun i x ->
                let fieldBuilder = Map.find x fieldBuilders
                ilg.Emit(OpCodes.Ldarg_0)
                emitLoadArg ilg (i + 1)
                ilg.Emit(OpCodes.Stfld, fieldBuilder))
            ilg.Emit(OpCodes.Ret)

            recordTypeBuilder.CreateType() |> ignore
        | IlFunction (name, parameters, returnType, expr) ->
            let methodBuilder =
                typeBuilder.DefineMethod(
                    name,
                    MethodAttributes.Public
                    ||| MethodAttributes.Static,
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
        let moduleBuilder =
            assemblyBuilder.DefineDynamicModule(ilModule.name, ilModule.name + ".dll", true)

        let typeBuilder =
            moduleBuilder.DefineType(
                ilModule.name,
                TypeAttributes.Public
                ||| TypeAttributes.Class)

        List.iter (declCodegen typeBuilder) ilModule.decls

        typeBuilder.CreateType() |> ignore

        match options with
        | Save (fileName) -> assemblyBuilder.Save (Path.GetFileName fileName)
        | _ -> ()

        assemblyBuilder
