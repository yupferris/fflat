module FFlat.Compiler.Codegen.Main
    open System
    open System.IO
    open System.Reflection
    open System.Reflection.Emit
    open System.Diagnostics
    open System.Runtime.Versioning
    open FFlat.Compiler.Common
    open FFlat.Compiler.Il
    open FFlat.Compiler.Codegen.Common
    open FFlat.Compiler.Codegen.Records

    type CodeGenOptions =
        | RunOnly
        | Save of string

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
            createRecordType name members typeBuilder |> ignore
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
            | Save (outputDir) ->
                let assemblyName =
                    new AssemblyName(ilModule.name)
                appDomain.DefineDynamicAssembly(
                    assemblyName,
                    AssemblyBuilderAccess.RunAndSave,
                    outputDir)
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
        let moduleFileName = ilModule.name + ".dll"
        let moduleBuilder =
            assemblyBuilder.DefineDynamicModule(ilModule.name, moduleFileName)

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
        | Save _ -> assemblyBuilder.Save moduleFileName
        | _ -> ()

        assemblyBuilder
