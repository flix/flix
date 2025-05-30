pub struct Board[r: Region] {
    r: Region[r],
    arr: Array[Symbol, r]
}

mod Board {

    pub def newBoard(rc: Region[r]): Board[r] \ r = {
        new Board @ rc {r = rc, arr = Array.repeat(rc, 9, Symbol.Empty)}
    }

    pub def getArr(board: Board[r]): Array[Symbol, r] = {
        board->arr
    }

    pub def makeMove(board: Board[r], symbol: Symbol, index: Int32): (Board[r], Bool) \ r = {
        if (Array.get(index, board->arr) == Symbol.Empty) {
            Array.put(symbol, index, board->arr);
            (board, true)
        }
        else {
           (board, false)
        }
    }

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

    pub def boardFilled(board: Board[r]): Bool \ r = {
        Array.forAll(sym -> sym != Symbol.Empty, board->arr)
    }

}
