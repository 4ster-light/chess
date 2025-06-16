module Types

type PieceType =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Color =
    | White
    | Black

type Piece = { PieceType: PieceType; Color: Color }

type Square = int * int // (row, col) from (0,0) to (7,7)

type Board = Map<Square, Piece>
