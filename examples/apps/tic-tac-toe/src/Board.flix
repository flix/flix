///
/// Struct representing the board, backed by a mutable array.
///

pub struct Board[r: Region] {
    r: Region[r],
    arr: Array[Symbol, r]
}


mod Board {

    ///
    /// Returns a new, empty tic-tac-toe board.
    ///
    pub def newBoard(rc: Region[r]): Board[r] \ r = {
        new Board @ rc {r = rc, arr = Array.repeat(rc, 9, Symbol.Empty)}
    }

    ///
    /// Returns the current state of the board as an array.
    ///
    pub def getArr(board: Board[r]): Array[Symbol, r] = {
        board->arr
    }

    ///
    /// Makes a move on the board at the specified index with the given symbol.
    ///
    /// Returns a tuple containing the updated board and a boolean indicating whether the move was successful.
    ///
    pub def makeMove(board: Board[r], symbol: Symbol, index: Int32): (Board[r], Bool) \ r = {
        if (Array.get(index, board->arr) == Symbol.Empty) {
            Array.put(symbol, index, board->arr);
            (board, true)
        }
        else {
           (board, false)
        }
    }

    ///
    /// Checks if the player with the specified symbol has won the game on the given board.
    ///
    pub def checkWin(board: Board[r], symbol: Symbol): Bool \ r  = {
        if ((Array.get(0, board->arr) == symbol)
		and (Array.get(1, board->arr) == symbol)
		and (Array.get(2, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(3, board->arr) == symbol)
		and (Array.get(4, board->arr) == symbol)
		and (Array.get(5, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(6, board->arr) == symbol)
		and (Array.get(7, board->arr) == symbol)
		and (Array.get(8, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(0, board->arr) == symbol)
		and (Array.get(3, board->arr) == symbol)
		and (Array.get(6, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(1, board->arr) == symbol)
		and (Array.get(4, board->arr) == symbol)
		and (Array.get(7, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(2, board->arr) == symbol)
		and (Array.get(5, board->arr) == symbol)
		and (Array.get(8, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(0, board->arr) == symbol)
		and (Array.get(4, board->arr) == symbol)
		and (Array.get(8, board->arr) == symbol)) {
            true
        }
        else if ((Array.get(2, board->arr) == symbol)
		and (Array.get(4, board->arr) == symbol)
		and (Array.get(6, board->arr) == symbol)) {
            true
        }

        else {
            false
        }

    }

    ///
    /// Checks if all cells are already filled on the board.
    ///
    pub def boardFilled(board: Board[r]): Bool \ r = {
        Array.forAll(sym -> sym != Symbol.Empty, board->arr)
    }

    ///
    /// Returns a string representation of the board.
    ///
    pub def toString(board: Board[r]): String \ r =
                    "       |       |       "
                    ++ "\n   ${Symbol.toString(Array.get(0, board->arr)) }   |   ${Symbol.toString(Array.get(1, board->arr)) }   |   ${Symbol.toString(Array.get(2, board->arr)) }   "
                    ++ "\n - - - | - - - | - - - "
                    ++ "\n   ${Symbol.toString(Array.get(3, board->arr)) }   |   ${Symbol.toString(Array.get(4, board->arr)) }   |   ${Symbol.toString(Array.get(5, board->arr)) }   "
                    ++ "\n - - - | - - - | - - - "
                    ++ "\n   ${Symbol.toString(Array.get(6, board->arr)) }   |   ${Symbol.toString(Array.get(7, board->arr)) }   |   ${Symbol.toString(Array.get(8, board->arr)) }   "
                    ++ "\n       |       |       "
}
