module FFlat.Compiler.Codegen.Records
    open System
    open System.Reflection
    open System.Reflection.Emit
    open System.Diagnostics
    open FFlat.Compiler.Il
    open FFlat.Compiler.Codegen.Common

    let private createTypeBuilder
        name
        (typeBuilder : TypeBuilder) =
        let recordTypeBuilder =
            typeBuilder.DefineNestedType(
                name,
                TypeAttributes.NestedPublic |||
                TypeAttributes.Class |||
                TypeAttributes.Sealed)
        addTypeAttr<CompilationMappingAttribute>
            recordTypeBuilder
            [|SourceConstructFlags.RecordType|]
        recordTypeBuilder

    let private createBackingFields
        (typeBuilder : TypeBuilder)
        (members : IlRecordMember list) =
        members
        |> List.map (fun x ->
            (x,
                let fieldBuilder =
                    typeBuilder.DefineField(
                        x.name + "@",
                        ilTypeToMsilType x.type',
                        FieldAttributes.Assembly)
                addFieldAttr<DebuggerBrowsableAttribute>
                    fieldBuilder
                    [|DebuggerBrowsableState.Never|]
                fieldBuilder))
        |> Map.ofList

    let private createGetterProps
        (typeBuilder : TypeBuilder)
        (fieldBuilders : Map<IlRecordMember, FieldBuilder>) =
        List.iteri (fun i member' ->
            let fieldBuilder = Map.find member' fieldBuilders
            let type' = fieldBuilder.FieldType
            let propBuilder =
                typeBuilder.DefineProperty(
                    member'.name,
                    PropertyAttributes.HasDefault,
                    type',
                    null)
            addPropAttr<CompilationMappingAttribute>
                propBuilder
                [|SourceConstructFlags.Field; i|]
            let getter =
                typeBuilder.DefineMethod(
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

    let private createCtor
        (typeBuilder : TypeBuilder)
        (fieldBuilders : Map<IlRecordMember, FieldBuilder>)
        members =
        let ctor =
            typeBuilder.DefineConstructor(
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
        let objCtor = getCtor typeof<obj> Type.EmptyTypes
        ilg.Emit(OpCodes.Call, objCtor)
        members
        |> List.iteri (fun i x ->
            let fieldBuilder = Map.find x fieldBuilders
            ilg.Emit(OpCodes.Ldarg_0)
            emitLoadArg ilg (i + 1)
            ilg.Emit(OpCodes.Stfld, fieldBuilder))
        ilg.Emit(OpCodes.Ret)

    let createRecordType name members typeBuilder =
        let recordTypeBuilder = createTypeBuilder name typeBuilder
        let fieldBuilders = createBackingFields recordTypeBuilder members
        createGetterProps recordTypeBuilder fieldBuilders members
        createCtor recordTypeBuilder fieldBuilders members
        recordTypeBuilder.CreateType()
