mod Interface {

    pub eff Interface {
        def display(board: String, msg: String): Unit
        def getMove(): Result[String, String]
    }

    pub def interfaceWithIOAndConsole(f: Unit -> a \ ef): a \ ef - Interface + IO + Console =
        run {
            f()
        }
        with handler Interface {
            def display(board, msg, resume) = {
                if (not String.isEmpty(board)) {
                    println("");
                    println(board);
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
