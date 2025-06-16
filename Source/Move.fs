module Move

open Types

let algebraicToSquare (s: string) =
    let file = int (s[0] - 'a')
    let rank = 7 - int (s[1] - '1')
    rank, file

let applyMove (board: Board) (moveStr: string) : Board =
    if moveStr.Length <> 4 then
        board
    else
        let fromSq = algebraicToSquare moveStr[0..1]
        let toSq = algebraicToSquare moveStr[2..3]

        match board.TryFind fromSq with
        | Some piece -> board |> Map.remove fromSq |> Map.add toSq piece
        | None -> board
