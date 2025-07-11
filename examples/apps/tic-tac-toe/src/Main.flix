///
/// The core gameplay loop called repeatedly until the game ends.
///
def gameLoop(board: Board, symbol: Symbol): Unit \ Interface = {
    Interface.display(board, "Enter a move (0 - 8): ");
    let move = Interface.getMove();
    match Board.makeMove(board, symbol, move) {
        case Some(newBoard) => {
            if(Board.checkWin(newBoard, symbol)) {
                match symbol {
                    case Symbol.X => Interface.display(newBoard, "Player One wins!!!")
                    case Symbol.O => Interface.display(newBoard, "Player Two wins!!!")
                    case _ => ()
                }
            } else if(Board.boardFilled(newBoard)) {
                Interface.display(board, "Draw")
            } else {
                gameLoop(newBoard, Symbol.complement(symbol))
            }
        }
        case None => {
            Interface.display(board, "Cell already filled");
            gameLoop(board, symbol)
        }
    }
}

///
/// The main entry point of the application.
///
/// Creates a new game board and starts the game loop.
///
def main(): Unit \ IO = {
    run {
        println("");
        println("  _______ _          _______             _______           ");
        println(" |__   __(_)        |__   __|           |__   __|        ");
        println("    | |   _  ___       | | __ _  ___       | | ___   ___ ");
        println("    | |  | |/ __|      | |/ _` |/ __|      | |/ _ \\ / _ \\");
        println("    | |  | | (__       | | (_| | (__       | | (_) |  __/");
        println("    |_|  |_|\\___|      |_|\\__,_|\\___|      |_|\\___/ \\___|");
        println("");
        println("Instructions:");
        println("The cells are numbered as follows:\n");
        println("   0   |   1   |   2   " );
        println(" - - - | - - - | - - - ");
        println("   3   |   4   |   5   " );
        println(" - - - | - - - | - - - ");
        println("   6   |   7   |   8   " );
        println("       |       |       ");
        println("Enter your move as one these numbers corresponding to a cell");
        println("");
        let board = Board.newBoard();
        gameLoop(board, Symbol.X)
    } with Interface.interfaceWithConsole
    with Console.runWithIO
}
