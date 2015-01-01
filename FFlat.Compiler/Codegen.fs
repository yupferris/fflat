module FFlat.Compiler.Codegen
    open System
    open System.Reflection
    open System.Reflection.Emit
    open FFlat.Compiler.Common
    open FFlat.Compiler.Il

    let binOpToOpCode = function
        | Add -> OpCodes.Add
        | Mul -> OpCodes.Mul

    let rec exprCodegen (ilg : ILGenerator) = function
        | IlBinOpExpression (op, left, right) ->
            exprCodegen ilg left
            exprCodegen ilg right
            ilg.Emit(binOpToOpCode op)

        | IlLiteral (value) -> ilg.Emit(OpCodes.Ldc_I4, value)

    let declCodegen (typeBuilder : TypeBuilder) = function
        | IlFunction (name, _, expr) ->
            let methodBuilder =
                typeBuilder.DefineMethod(
                    name,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<int>,
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
