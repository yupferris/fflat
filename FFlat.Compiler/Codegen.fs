module FFlat.Compiler.Codegen
    open System
    open System.IO
    open System.Reflection
    open System.Reflection.Emit
    open System.Diagnostics
    open System.Runtime.Versioning
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

    let addAttr<'a> f parameters =
        let ctor =
            typeof<'a>.GetConstructor(
                Array.map (fun x -> x.GetType()) parameters)
        new CustomAttributeBuilder(ctor, parameters) |> f

    let addAttrWithProps<'a> f parameters setters =
        let type' = typeof<'a>
        let ctor =
            type'.GetConstructor(
                Array.map (fun x -> x.GetType()) parameters)
        new CustomAttributeBuilder(
            ctor,
            parameters,
            Array.map (fst >> type'.GetProperty) setters,
            Array.map snd setters)
        |> f

    let addAssemblyAttr<'a> (assemblyBuilder : AssemblyBuilder) =
        addAttr<'a> assemblyBuilder.SetCustomAttribute

    let addAssemblyAttrWithProps<'a> (assemblyBuilder : AssemblyBuilder) =
        addAttrWithProps<'a> assemblyBuilder.SetCustomAttribute

    let addTypeAttr<'a> (typeBuilder : TypeBuilder) =
        addAttr<'a> typeBuilder.SetCustomAttribute

    let addFieldAttr<'a> (fieldBuilder : FieldBuilder) =
        addAttr<'a> fieldBuilder.SetCustomAttribute

    let addPropAttr<'a> (propBuilder : PropertyBuilder) =
        addAttr<'a> propBuilder.SetCustomAttribute

    let addMethodAttr<'a> (methodBuilder : MethodBuilder) =
        addAttr<'a> methodBuilder.SetCustomAttribute

    let declCodegen (typeBuilder : TypeBuilder) = function
        | IlRecord (name, members) ->
            let recordTypeBuilder =
                typeBuilder.DefineNestedType(
                    name,
                    TypeAttributes.NestedPublic |||
                    TypeAttributes.Class |||
                    TypeAttributes.Sealed)
            addTypeAttr<CompilationMappingAttribute>
                recordTypeBuilder
                [|SourceConstructFlags.RecordType|]

            // Generate backing fields
            let fieldBuilders =
                members
                |> List.map (fun x ->
                    (x,
                        let fieldBuilder =
                            recordTypeBuilder.DefineField(
                                x.name + "@",
                                ilTypeToMsilType x.type',
                                FieldAttributes.Assembly)
                        addFieldAttr<DebuggerBrowsableAttribute>
                            fieldBuilder
                            [|DebuggerBrowsableState.Never|]
                        fieldBuilder))
                |> Map.ofList

            // Generate getter properties for backing fields
            members
            |> List.iteri (fun i member' ->
                let fieldBuilder = Map.find member' fieldBuilders
                let type' = fieldBuilder.FieldType
                let propBuilder =
                    recordTypeBuilder.DefineProperty(
                        member'.name,
                        PropertyAttributes.HasDefault,
                        type',
                        null)
                addPropAttr<CompilationMappingAttribute>
                    propBuilder
                    [|SourceConstructFlags.Field; i|]
                let getter =
                    recordTypeBuilder.DefineMethod(
                        "get_" + member'.name,
                        MethodAttributes.Public |||
                        MethodAttributes.SpecialName |||
                        MethodAttributes.HideBySig,
                        type',
                        Type.EmptyTypes)
                let ilg = getter.GetILGenerator()
                ilg.Emit(OpCodes.Ldarg_0)
                ilg.Emit(OpCodes.Ldfld, fieldBuilder)
                ilg.Emit(OpCodes.Ret)
                propBuilder.SetGetMethod(getter))

            // Generate constructor
            let ctor =
                recordTypeBuilder.DefineConstructor(
                    MethodAttributes.Public |||
                    MethodAttributes.HideBySig,
                    CallingConventions.Standard,
                    members
                    |> List.map (fun x -> (Map.find x fieldBuilders).FieldType)
                    |> Array.ofList)
            members
            |> List.iteri (fun i x ->
                ctor.DefineParameter(
                    i + 1,
                    ParameterAttributes.None,
                    x.name)
                |> ignore)
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
            let hasSingleUnitParam =
                parameters.Length = 1 &&
                List.forall
                    (function
                        | IlUnitParameter -> true
                        | _ -> false)
                    parameters
            let methodBuilder =
                typeBuilder.DefineMethod(
                    name,
                    MethodAttributes.Public
                    ||| MethodAttributes.Static,
                    ilTypeToFunctionReturnMsilType returnType,
                    if hasSingleUnitParam then [||]
                    else List.map ilParameterToMsilType parameters |> Array.ofList)
            if not hasSingleUnitParam then
                if parameters.Length > 1 then
                    addMethodAttr<CompilationArgumentCountsAttribute>
                        methodBuilder
                        [|
                            (parameters
                            |> List.map (fun _ -> 1)
                            |> Array.ofList)
                        |]
                parameters
                |> List.iteri
                    (fun i ->
                        function
                        | IlNamedParameter (name, _) ->
                            methodBuilder.DefineParameter(
                                i + 1,
                                ParameterAttributes.None,
                                name)
                            |> ignore
                        | _ -> ())
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
        addAssemblyAttr<FSharpInterfaceDataVersionAttribute>
            assemblyBuilder
            [|2; 0; 0|]
        // I don't really know what this one does, but I'm putting it in here
        // since fsc is dumping assemblies with this attached. Will look into
        // it more later.
        addAssemblyAttr<DebuggableAttribute>
            assemblyBuilder
            [|DebuggableAttribute.DebuggingModes.IgnoreSymbolStoreSequencePoints|]
        addAssemblyAttrWithProps<TargetFrameworkAttribute>
            assemblyBuilder
            [|".NETFramework,Version=v4.5"|]
            [|("FrameworkDisplayName", ".NET Framework 4.5" :> obj)|]
        let moduleBuilder =
            assemblyBuilder.DefineDynamicModule(ilModule.name, ilModule.name + ".dll", true)

        let typeBuilder =
            moduleBuilder.DefineType(
                ilModule.name,
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed)
        addTypeAttr<CompilationMappingAttribute>
            typeBuilder
            [|SourceConstructFlags.Module|]

        List.iter (declCodegen typeBuilder) ilModule.decls

        typeBuilder.CreateType() |> ignore

        match options with
        | Save (fileName) -> assemblyBuilder.Save (Path.GetFileName fileName)
        | _ -> ()

        assemblyBuilder
