mod Interface {

    pub eff Interface {
        def display(board: List[Symbol], msg: String): Unit
        def getMove(): Result[String, String]
    }

    pub def interfaceWithIOAndConsole(f: Unit -> a \ ef): a \ ef - Interface + IO + Console = 
        run {
            f()
        }
        with handler Interface {
            def display(board, msg, resume) = region rc {
                if (not List.isEmpty(board)) {
                    let arr = List.toArray(rc, board);
                    println("");
                    println("       |       |       ");
                    println("   " ++ Symbol.toString(Array.get(0, arr)) ++ "   |   " ++ Symbol.toString(Array.get(1, arr)) ++ "   |   " ++ Symbol.toString(Array.get(2, arr)) ++ "   " );
                    println(" - - - | - - - | - - - ");
                    println("   " ++ Symbol.toString(Array.get(3, arr)) ++ "   |   " ++ Symbol.toString(Array.get(4, arr)) ++ "   |   " ++ Symbol.toString(Array.get(5, arr)) ++ "   " );
                    println(" - - - | - - - | - - - ");
                    println("   " ++ Symbol.toString(Array.get(6, arr)) ++ "   |   " ++ Symbol.toString(Array.get(7, arr)) ++ "   |   " ++ Symbol.toString(Array.get(8, arr)) ++ "   " );
                    println("       |       |       ");
                    println("");
                    println(msg);
                    resume()
                } else {
                    println("");
                    println(msg);
                    resume()
                }
            }
            def getMove(resume) = {
                let line = Console.readln();
                if (Object.isNull(line)) 
                    resume(Result.Err("no input"))
                else 
                    resume(Result.Ok(line))
            }
        }
}
