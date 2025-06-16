open Types
open Board
open Move

[<EntryPoint>]
let main _ =
    let rec loop (board: Board) =
        printBoard board
        printfn "Enter your move (e.g., e2e4):"

        let input = System.Console.ReadLine()

        match input with
        | null
        | "quit" -> 0
        | moveStr ->
            let newBoard = applyMove board moveStr
            loop newBoard

    loop initialPosition
