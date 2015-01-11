module FFlat.Compiler.Compiler
    open FFlat.Compiler.Parser
    open FFlat.Compiler.Il
    open FFlat.Compiler.Optimization
    open FFlat.Compiler.Codegen

    let compile =
        parseModule
        >> buildIl
        >> checkTypes
        >> foldConstants
        >> codegen
