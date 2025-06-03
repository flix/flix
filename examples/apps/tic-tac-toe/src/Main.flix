def readAndParseMove(): Result[String, Int32] \ {Interface.Interface} =
    forM(m <- Interface.Interface.getMove();
         n <- Int32.parse(10, m)
    ) yield n

def gameLoop(board: Board[r], symbol: Symbol): Unit \ {IO + r + Interface.Interface} = {
    Interface.Interface.display(Array.toList(Board.getArr(board)), "Enter a move (0 - 8): ");
    match readAndParseMove() {
        case Result.Ok(index) =>
            if(not(index >= 0 and index <= 8)) {
                Interface.Interface.display(List.Nil, "Invalid move");
                gameLoop(board, symbol)
            } else {
                let (newBoard, moveSuccess) = Board.makeMove(board, symbol, index);
                if (moveSuccess) {
                    if(Board.checkWin(newBoard, symbol)) {
                        match symbol {
                            case Symbol.X => Interface.Interface.display(Array.toList(Board.getArr(board)), "Player One wins!!!")
                            case Symbol.O => Interface.Interface.display(Array.toList(Board.getArr(board)), "Player Two wins!!!")
                            case _ => ()
                        }
                    } else if(Board.boardFilled(newBoard)) {
                        Interface.Interface.display(Array.toList(Board.getArr(board)), "Draw")
                    } else {
                        gameLoop(newBoard, Symbol.complement(symbol))
                    }
                } else {
                    Interface.Interface.display(List.Nil, "Cell already filled");
                    gameLoop(board, symbol)
                }
            }
        case Result.Err(_) => Interface.Interface.display(List.Nil, "Invalid move")
    }
}

def main(): Unit \ IO = region rc1 {
    run {
        println("");
        println("_______ _          _______             _______           ");
        println(" |__   __(_)        |__   __|           |__   __|        ");
        println("    | |   _  ___ ______| | __ _  ___ ______| | ___   ___ ");
        println("    | |  | |/ __|______| |/ _` |/ __|______| |/ _ \\ / _ \\");
        println("    | |  | | (__       | | (_| | (__       | | (_) |  __/");
        println("    |_|  |_|\\___|      |_|\\__,_|\\___|      |_|\\___/ \\___|");
        println("");
        println("Instructions:");
        println("The cells are numbered as follows:\n");
        println("   " ++ "0" ++ "   |   " ++ "1" ++ "   |   " ++ "2" ++ "   " );
        println(" - - - | - - - | - - - ");
        println("   " ++ "3" ++ "   |   " ++ "4" ++ "   |   " ++ "5" ++ "   " );
        println(" - - - | - - - | - - - ");
        println("   " ++ "6" ++ "   |   " ++ "7" ++ "   |   " ++ "8" ++ "   " );
        println("       |       |       ");
        println("Enter your move as one these numbers corresponding to a cell");
        println("");
        let board = Board.newBoard(rc1);
        gameLoop(board, Symbol.X)
    } with Interface.interfaceWithIOAndConsole
    with Console.runWithIO
}
