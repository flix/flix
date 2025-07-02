///
/// Struct representing the board, backed by an immutable vector.
///

pub struct Board[r] {
    vec: Vector[Symbol]
}


mod Board {

    ///
    /// Returns a new, empty tic-tac-toe board.
    ///
    pub def newBoard(rc: Region[r]): Board[r] \ r = {
        new Board @ rc {vec = Vector.repeat(9, Symbol.Empty)}
    }

    ///
    /// Returns the current state of the board as a vector.
    ///
    pub def getVec(board: Board[r]): Vector[Symbol] = {
        board->vec
    }

    ///
    /// Makes a move on the board at the specified index with the given symbol.
    ///
    /// Returns a tuple containing the updated board and a boolean indicating whether the move was successful.
    ///
    pub def makeMove(board: Board[r], symbol: Symbol, index: Int32, rc: Region[r]): (Board[r], Bool) \ r = {
        if (Vector.get(index, board->vec) == Symbol.Empty) {
            let newBoard = new Board @ rc {vec = Vector.update(index, symbol, board->vec)};
            (newBoard, true)
        }
        else {
           (board, false)
        }
    }

    ///
    /// Checks if the player with the specified symbol has won the game on the given board.
    ///
    pub def checkWin(board: Board[r], symbol: Symbol): Bool = {
        if ((Vector.get(0, board->vec) == symbol)
		and (Vector.get(1, board->vec) == symbol)
		and (Vector.get(2, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(3, board->vec) == symbol)
		and (Vector.get(4, board->vec) == symbol)
		and (Vector.get(5, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(6, board->vec) == symbol)
		and (Vector.get(7, board->vec) == symbol)
		and (Vector.get(8, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(0, board->vec) == symbol)
		and (Vector.get(3, board->vec) == symbol)
		and (Vector.get(6, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(1, board->vec) == symbol)
		and (Vector.get(4, board->vec) == symbol)
		and (Vector.get(7, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(2, board->vec) == symbol)
		and (Vector.get(5, board->vec) == symbol)
		and (Vector.get(8, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(0, board->vec) == symbol)
		and (Vector.get(4, board->vec) == symbol)
		and (Vector.get(8, board->vec) == symbol)) {
            true
        }
        else if ((Vector.get(2, board->vec) == symbol)
		and (Vector.get(4, board->vec) == symbol)
		and (Vector.get(6, board->vec) == symbol)) {
            true
        }

        else {
            false
        }

    }

    ///
    /// Checks if all cells are already filled on the board.
    ///
    pub def boardFilled(board: Board[r]): Bool = {
        Vector.forAll(sym -> sym != Symbol.Empty, board->vec)
    }

    ///
    /// Returns a string representation of the board.
    ///
    pub def toString(board: Board[r]): String =
                    "       |       |       "
                    ++ "\n   ${Symbol.toString(Vector.get(0, board->vec)) }   |   ${Symbol.toString(Vector.get(1, board->vec)) }   |   ${Symbol.toString(Vector.get(2, board->vec)) }   "
                    ++ "\n - - - | - - - | - - - "
                    ++ "\n   ${Symbol.toString(Vector.get(3, board->vec)) }   |   ${Symbol.toString(Vector.get(4, board->vec)) }   |   ${Symbol.toString(Vector.get(5, board->vec)) }   "
                    ++ "\n - - - | - - - | - - - "
                    ++ "\n   ${Symbol.toString(Vector.get(6, board->vec)) }   |   ${Symbol.toString(Vector.get(7, board->vec)) }   |   ${Symbol.toString(Vector.get(8, board->vec)) }   "
                    ++ "\n       |       |       "
}
