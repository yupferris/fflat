#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

#load "Common.fs"
#load "Ast.fs"
#load "Parser.fs"
#load "Il.fs"
#load "Optimization.fs"
#load "Codegen.fs"
#load "Compiler.fs"

open FFlat.Compiler.Codegen
open FFlat.Compiler.Compiler

let assembly =
    compile (Save "C:\\Users\\Ferris\\Documents\\derp.dll") (*RunOnly*) @"

module FirstVertical =
    begin
        let func0 () = 2 + 3 * 4
        let func1 () = 2 * (((((3))))) + 4
        let func2 () = ()
        let func3 (x : int) = x + 5
        let func4 (x : int) (y : int) = x * y + 5
        let func5 (jake : int) (rules : int) (hard : int) = jake + rules + hard
        let func6 (a : int) (b : int) (c : int) (d : int) = 12

        type SomeType = { x : int; y : int }
    end

    "

[
    [||]
    [||]
    [||]
    [|2 :> obj|]
    [|3 :> obj; 4 :> obj|]
    [|-1 :> obj; -2 :> obj; -3 :> obj|]
    [|1 :> obj; 2 :> obj; 3 :> obj; 4 :> obj|]
]
|> List.mapi (fun i x ->
    assembly
        .GetType("FirstVertical")
        .GetMethod("func" + i.ToString())
        .Invoke(null, x))
