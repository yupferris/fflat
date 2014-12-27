#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

#load "Common.fs"
#load "Ast.fs"
#load "Parser.fs"
#load "Il.fs"
#load "Optimization.fs"
#load "Codegen.fs"

open FFlat.Compiler.Common
open FFlat.Compiler.Ast
open FFlat.Compiler.Parser
open FFlat.Compiler.Il
open FFlat.Compiler.Optimization
open FFlat.Compiler.Codegen

// Test
let print x =
    printfn "%A" x
    x

let assembly =
    parseModule @"

module FirstVertical =
    begin
        let firstFunc () = 2 + 3 * 4
        let secondFunc () = 2 * 3 + 4
    end

    "
    |> moduleBuildIl
    |> print
    |> foldConstants
    |> print
    |> codegen

["firstFunc"; "secondFunc"]
|> List.map (fun x -> assembly.GetType("FirstVertical").GetMethod(x).Invoke(null, [||]))
