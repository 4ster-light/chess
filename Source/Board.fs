module Board

open Types

let initialPosition: Board =
    let backRank color row =
        [ Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook ]
        |> List.mapi (fun col pt -> (row, col), { PieceType = pt; Color = color })

    let pawnRank color row =
        [ 0..7 ]
        |> List.map (fun col -> ((row, col), { PieceType = Pawn; Color = color }))

    let white = backRank White 7 @ pawnRank White 6
    let black = backRank Black 0 @ pawnRank Black 1
    Map.ofList (white @ black)

let pieceToChar piece =
    let ch =
        match piece.PieceType with
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'Q'
        | King -> 'K'

    if piece.Color = White then ch else System.Char.ToLower ch

let printBoard (board: Board) =
    for row = 0 to 7 do
        for col = 0 to 7 do
            match board.TryFind(row, col) with
            | Some piece -> printf $"%c{pieceToChar piece} "
            | None -> printf ". "

        printfn ""
