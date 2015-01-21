module FFlat.Compiler.Codegen.Common
    open System
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

    let getCtor (type' : Type) parameters =
        type'.GetConstructor(
            Array.map (fun x -> x.GetType()) parameters)

    let addAttr<'a> f parameters =
        let ctor = getCtor typeof<'a> parameters
        new CustomAttributeBuilder(ctor, parameters) |> f

    let addAttrWithProps<'a> f parameters setters =
        let type' = typeof<'a>
        let ctor = getCtor type' parameters
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
