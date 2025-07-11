///
/// Effect representing the user interface for the game.
///
pub eff Interface {

    ///
    /// Prints the current state of the board and a message.
    ///
    def display(board: Board, msg: String): Unit

    ///
    /// Gets a move from the user.
    ///
    def getMove(): Int32
}

mod Interface {

    ///
    /// Recursive helper function to read and return only valid moves.
    ///
    def getMoveFromConsole(): Int32 \ Console = {
        let line = Console.readln();
        if(Object.isNull(line)) {
            Console.println("Invalid move. Move must be a positive integer between 0 and 8");
            getMoveFromConsole()
        } else {
            match Int32.parse(10, line) {
                case Result.Ok(move) if (0 <= move and move <= 8) => move
                case _ => {
                    Console.println("Invalid move. Move must be a positive integer between 0 and 8");
                    getMoveFromConsole()
                }
            }
        }
    }


    ///
    /// Runs the Interface effect of the given function `f`.
    ///
    /// In other words, re-interprets the Interface effect using the IO and Console effects.
    ///
    pub def interfaceWithConsole(f: Unit -> a \ ef): a \ ef - Interface + Console =
        run {
            f()
        }
        with handler Interface {
            def display(board, msg, resume) = {
                Console.println("");
                Console.println(Board.toString(board));
                Console.println("");
                Console.println(msg);
                resume()
            }

            def getMove(resume) = {
                resume(getMoveFromConsole())
            }
        }
}
