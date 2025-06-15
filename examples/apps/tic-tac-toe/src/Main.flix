///
/// The core gameplay loop called repeatedly until the game ends.
///
def gameLoop(board: Board[r], symbol: Symbol): Unit \ {r + Interface} = {
    Interface.display(Board.toString(board), "Enter a move (0 - 8): ");
    let move = Interface.getMove();
    let (newBoard, moveSuccess) = Board.makeMove(board, symbol, move);
    if (moveSuccess) {
        if(Board.checkWin(newBoard, symbol)) {
            match symbol {
                case Symbol.X => Interface.display(Board.toString(board), "Player One wins!!!")
                case Symbol.O => Interface.display(Board.toString(board), "Player Two wins!!!")
                case _ => ()
            }
        } else if(Board.boardFilled(newBoard)) {
            Interface.display(Board.toString(board), "Draw")
        } else {
            gameLoop(newBoard, Symbol.complement(symbol))
        }
    } else {
        Interface.display("", "Cell already filled");
        gameLoop(board, symbol)
    }
}

///
/// The main entry point of the application.
///
/// Creates a new game board and starts the game loop.
///
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
