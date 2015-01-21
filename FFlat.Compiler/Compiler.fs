module FFlat.Compiler.Compiler
    open System.Reflection
    open FFlat.Compiler.Parser
    open FFlat.Compiler.Il
    open FFlat.Compiler.Optimization
    open FFlat.Compiler.Codegen.Main

    let compile codeGenOptions =
        parseModule
        >> buildIl
        >> checkTypes
        >> foldConstants
        >> codegen codeGenOptions
        >> (fun x -> x :> Assembly)
