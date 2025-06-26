type Color =
    | White
    | Black

type PieceType =
    | Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King

type Piece = { Type: PieceType; Color: Color }

type Square = int * int // (row, col) where 0,0 is top-left (a8)

type Move =
    { From: Square
      To: Square
      Piece: Piece
      CapturedPiece: Piece option
      IsPromotion: bool
      PromotionPiece: PieceType option }

type Board = Map<Square, Piece>

type GameState =
    { Board: Board
      ActiveColor: Color
      CastlingRights: Set<string> // "K", "Q", "k", "q"
      EnPassantTarget: Square option
      HalfMoveClock: int
      FullMoveNumber: int }

// Utility functions
let oppositeColor =
    function
    | White -> Black
    | Black -> White

let isValidSquare (row, col) =
    row >= 0 && row < 8 && col >= 0 && col < 8

let pieceValue =
    function
    | Pawn -> 100
    | Knight -> 300
    | Bishop -> 300
    | Rook -> 500
    | Queen -> 900
    | King -> 10000

// Initial board setup
let initialBoard =
    let pieces =
        [
          // White pieces
          yield (7, 0), { Type = Rook; Color = White }
          yield (7, 1), { Type = Knight; Color = White }
          yield (7, 2), { Type = Bishop; Color = White }
          yield (7, 3), { Type = Queen; Color = White }
          yield (7, 4), { Type = King; Color = White }
          yield (7, 5), { Type = Bishop; Color = White }
          yield (7, 6), { Type = Knight; Color = White }
          yield (7, 7), { Type = Rook; Color = White }
          // White pawns
          for col in 0..7 do
              yield (6, col), { Type = Pawn; Color = White }
          // Black pieces
          yield (0, 0), { Type = Rook; Color = Black }
          yield (0, 1), { Type = Knight; Color = Black }
          yield (0, 2), { Type = Bishop; Color = Black }
          yield (0, 3), { Type = Queen; Color = Black }
          yield (0, 4), { Type = King; Color = Black }
          yield (0, 5), { Type = Bishop; Color = Black }
          yield (0, 6), { Type = Knight; Color = Black }
          yield (0, 7), { Type = Rook; Color = Black }
          // Black pawns
          for col in 0..7 do
              yield ((1, col), { Type = Pawn; Color = Black }) ]

    Map.ofList pieces

let initialGameState =
    { Board = initialBoard
      ActiveColor = White
      CastlingRights = Set.ofList [ "K"; "Q"; "k"; "q" ]
      EnPassantTarget = None
      HalfMoveClock = 0
      FullMoveNumber = 1 }

// Move generation
let rec getMovesInDirection board (row, col) (dRow, dCol) color =
    let newRow, newCol = row + dRow, col + dCol
    let newSquare = (newRow, newCol)

    if not (isValidSquare newSquare) then
        []
    else
        match Map.tryFind newSquare board with
        | None -> newSquare :: getMovesInDirection board newSquare (dRow, dCol) color
        | Some piece when piece.Color <> color -> [ newSquare ] // Can capture
        | Some _ -> [] // Blocked by own piece

let getPawnMoves board (row, col) color =
    let direction = if color = White then -1 else 1
    let startRow = if color = White then 6 else 1
    let moves = ResizeArray<Square>()

    // Forward move
    let oneSquareAhead = (row + direction, col)

    if isValidSquare oneSquareAhead && not (Map.containsKey oneSquareAhead board) then
        moves.Add(oneSquareAhead)

        // Two squares ahead from starting position
        if row = startRow then
            let twoSquareAhead = (row + 2 * direction, col)

            if isValidSquare twoSquareAhead && not (Map.containsKey twoSquareAhead board) then
                moves.Add(twoSquareAhead)

    // Captures
    for deltaCol in [ -1; 1 ] do
        let captureSquare = (row + direction, col + deltaCol)

        if isValidSquare captureSquare then
            match Map.tryFind captureSquare board with
            | Some piece when piece.Color <> color -> moves.Add(captureSquare)
            | _ -> ()

    moves |> List.ofSeq

let getKnightMoves board (row, col) color =
    let knightMoves =
        [ (-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1) ]

    knightMoves
    |> List.map (fun (dRow, dCol) -> (row + dRow, col + dCol))
    |> List.filter isValidSquare
    |> List.filter (fun square ->
        match Map.tryFind square board with
        | Some piece -> piece.Color <> color
        | None -> true)

let getBishopMoves board (row, col) color =
    let directions = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]

    directions
    |> List.collect (fun (dRow, dCol) -> getMovesInDirection board (row, col) (dRow, dCol) color)

let getRookMoves board (row, col) color =
    let directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

    directions
    |> List.collect (fun (dRow, dCol) -> getMovesInDirection board (row, col) (dRow, dCol) color)

let getQueenMoves board (row, col) color =
    let directions =
        [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

    directions
    |> List.collect (fun (dRow, dCol) -> getMovesInDirection board (row, col) (dRow, dCol) color)

let getKingMoves board (row, col) color =
    let kingMoves =
        [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

    kingMoves
    |> List.map (fun (dRow, dCol) -> (row + dRow, col + dCol))
    |> List.filter isValidSquare
    |> List.filter (fun square ->
        match Map.tryFind square board with
        | Some piece -> piece.Color <> color
        | None -> true)

let getPieceMoves board square piece =
    match piece.Type with
    | Pawn -> getPawnMoves board square piece.Color
    | Knight -> getKnightMoves board square piece.Color
    | Bishop -> getBishopMoves board square piece.Color
    | Rook -> getRookMoves board square piece.Color
    | Queen -> getQueenMoves board square piece.Color
    | King -> getKingMoves board square piece.Color

// Check if a square is under attack
let isSquareUnderAttack board square color =
    board
    |> Map.exists (fun fromSquare piece ->
        piece.Color <> color
        && (getPieceMoves board fromSquare piece |> List.contains square))

// Find the king's position
let findKing board color =
    board
    |> Map.tryFindKey (fun _ piece -> piece.Type = King && piece.Color = color)

// Check if the king is in check
let isInCheck gameState =
    match findKing gameState.Board gameState.ActiveColor with
    | Some kingSquare -> isSquareUnderAttack gameState.Board kingSquare gameState.ActiveColor
    | None -> false

// Generate all legal moves
let generateMoves gameState =
    let moves = ResizeArray<Move>()

    for fromSquare, piece in Map.toList gameState.Board do
        if piece.Color = gameState.ActiveColor then
            let possibleMoves = getPieceMoves gameState.Board fromSquare piece

            for toSquare in possibleMoves do
                let capturedPiece = Map.tryFind toSquare gameState.Board

                let move =
                    { From = fromSquare
                      To = toSquare
                      Piece = piece
                      CapturedPiece = capturedPiece
                      IsPromotion = false
                      PromotionPiece = None }

                moves.Add(move)

    moves |> List.ofSeq

// Make a move
let makeMove gameState move =
    let newBoard = gameState.Board |> Map.remove move.From |> Map.add move.To move.Piece

    { gameState with
        Board = newBoard
        ActiveColor = oppositeColor gameState.ActiveColor
        HalfMoveClock = gameState.HalfMoveClock + 1
        FullMoveNumber =
            if gameState.ActiveColor = Black then
                gameState.FullMoveNumber + 1
            else
                gameState.FullMoveNumber }

// Evaluation function
let evaluateBoard gameState =
    let mutable score = 0

    for _, piece in Map.toList gameState.Board do
        let pieceScore = pieceValue piece.Type

        if piece.Color = White then
            score <- score + pieceScore
        else
            score <- score - pieceScore

    score

// Minimax with alpha-beta pruning
let rec minimax gameState depth alpha beta maximizingPlayer =
    if depth = 0 then
        evaluateBoard gameState
    else
        let moves = generateMoves gameState

        if List.isEmpty moves then
            if isInCheck gameState then
                if maximizingPlayer then -10000 else 10000
            else
                0 // Stalemate
        else if maximizingPlayer then
            let mutable maxEval = -10000
            let mutable alpha = alpha
            let mutable breakFlag = false

            for move in moves do
                if not breakFlag then
                    let newGameState = makeMove gameState move
                    let eval = minimax newGameState (depth - 1) alpha beta false
                    maxEval <- max maxEval eval
                    alpha <- max alpha eval

                    if beta <= alpha then
                        breakFlag <- true

            maxEval
        else
            let mutable minEval = 10000
            let mutable beta = beta
            let mutable breakFlag = false

            for move in moves do
                if not breakFlag then
                    let newGameState = makeMove gameState move
                    let eval = minimax newGameState (depth - 1) alpha beta true
                    minEval <- min minEval eval
                    beta <- min beta eval

                    if beta <= alpha then
                        breakFlag <- true

            minEval

let findBestMove gameState depth =
    let moves = generateMoves gameState

    if List.isEmpty moves then
        None
    else
        let mutable bestMove = List.head moves
        let mutable bestScore = if gameState.ActiveColor = White then -10000 else 10000

        for move in moves do
            let newGameState = makeMove gameState move
            let score = minimax newGameState depth -10000 10000 (gameState.ActiveColor = Black)

            if
                gameState.ActiveColor = White && score > bestScore
                || gameState.ActiveColor = Black && score < bestScore
            then
                bestScore <- score
                bestMove <- move

        Some bestMove

// Display functions
let pieceToChar piece =
    match piece.Type, piece.Color with
    | Pawn, White -> 'P'
    | Pawn, Black -> 'p'
    | Rook, White -> 'R'
    | Rook, Black -> 'r'
    | Knight, White -> 'N'
    | Knight, Black -> 'n'
    | Bishop, White -> 'B'
    | Bishop, Black -> 'b'
    | Queen, White -> 'Q'
    | Queen, Black -> 'q'
    | King, White -> 'K'
    | King, Black -> 'k'

let displayBoard board =
    printfn "  a b c d e f g h"

    for row in 0..7 do
        printf $"%d{8 - row} "

        for col in 0..7 do
            match Map.tryFind (row, col) board with
            | Some piece -> printf $"%c{pieceToChar piece} "
            | None -> printf ". "

        printfn $"%d{8 - row}"

    printfn "  a b c d e f g h"

let squareToString (row, col) =
    let file = char (int 'a' + col)
    let rank = 8 - row
    $"%c{file}%d{rank}"

let displayMove move =
    $"%s{squareToString move.From}%s{squareToString move.To}"

[<EntryPoint>]
let main _ =
    let mutable gameState = initialGameState
    let mutable gameOver = false

    while not gameOver do
        displayBoard gameState.Board
        printfn $"\nCurrent player: %A{gameState.ActiveColor}"

        if isInCheck gameState then
            printfn "Check!"

        let moves = generateMoves gameState

        if List.isEmpty moves then
            if isInCheck gameState then
                printfn $"Checkmate! %A{oppositeColor gameState.ActiveColor} wins!"
            else
                printfn "Stalemate!"

            gameOver <- true
        else
            match findBestMove gameState 3 with
            | Some move ->
                printfn $"Engine plays: %s{displayMove move}"
                gameState <- makeMove gameState move
            | None ->
                printfn "No legal moves available"
                gameOver <- true

    0
