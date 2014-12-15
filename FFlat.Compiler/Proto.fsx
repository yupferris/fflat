#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

open FParsec

type Expression =
    | IntLiteral of int

let code = @"80085"

let integer = pint32
let expression = integer |>> IntLiteral

match run expression code with
| Success (result, _, _) -> printfn "Expression: %A" result
| Failure (message, _, _) -> printfn "Failure: %s" message
