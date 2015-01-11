module FFlat.Compiler.Common
    type BinOp =
        | Add
        | Mul

    let tryFindIndexItem f l =
        let rec aux index = function
            | [] -> None
            | head :: tail ->
                match f head with
                | true -> Some (index, head)
                | _ -> aux (index + 1) tail
        aux 0 l
