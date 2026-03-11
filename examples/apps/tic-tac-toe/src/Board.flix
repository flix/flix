///
/// Enum representing the board, backed by an immutable vector.
///

enum Board(Vector[Symbol])

mod Board {

    ///
    /// Returns a new, empty tic-tac-toe board.
    ///
    pub def newBoard(): Board = {
        Board(Vector.repeat(9, Symbol.Empty))
    }

    ///
    /// Makes a move on the board at the specified index with the given symbol.
    ///
    /// Returns the updated board if the move is valid, otherwise returns the original board.
    ///
    pub def makeMove(board: Board, symbol: Symbol, index: Int32): Option[Board]  = match board {
        case Board(vec) => {
            if (Vector.get(index, vec) == Symbol.Empty) {
                let newBoard = Board(Vector.update(index, symbol, vec));
                Some(newBoard)
            }
            else {
               None
            }
        }
    }

    ///
    /// Checks if the player with the specified symbol has won the game on the given board.
    ///
    pub def checkWin(board: Board, symbol: Symbol): Bool = match board {
        case Board(vec) => {
            let patterns = (0, 1, 2) :: (3, 4, 5) :: (6, 7, 8) :: (0, 3, 6) :: (1, 4, 7) :: (2, 5, 8) :: (0, 4, 8) :: (2, 4, 6) :: Nil;
            List.exists((pattern: (Int32, Int32, Int32)) -> {
                    match pattern {
                        case (a, b, c) =>
                            (Vector.get(a, vec) == symbol)
                            and (Vector.get(b, vec) == symbol)
                            and (Vector.get(c, vec) == symbol)
                    }
            }, patterns)
        }
    }

    ///
    /// Checks if all cells are already filled on the board.
    ///
    pub def boardFilled(board: Board): Bool = match board {
        case Board(vec) =>
            Vector.forAll(sym -> sym != Symbol.Empty, vec)
    }

    ///
    /// Returns a string representation of the board.
    ///
    pub def toString(board: Board): String = match board {
        case Board(vec) =>  "       |       |       "
                            ++ "\n   ${Symbol.toString(Vector.get(0, vec)) }   |   ${Symbol.toString(Vector.get(1, vec)) }   |   ${Symbol.toString(Vector.get(2, vec)) }   "
                            ++ "\n - - - | - - - | - - - "
                            ++ "\n   ${Symbol.toString(Vector.get(3, vec)) }   |   ${Symbol.toString(Vector.get(4, vec)) }   |   ${Symbol.toString(Vector.get(5, vec)) }   "
                            ++ "\n - - - | - - - | - - - "
                            ++ "\n   ${Symbol.toString(Vector.get(6, vec)) }   |   ${Symbol.toString(Vector.get(7, vec)) }   |   ${Symbol.toString(Vector.get(8, vec)) }   "
                            ++ "\n       |       |       "
        }
}
