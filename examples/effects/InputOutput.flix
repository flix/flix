mod InputOutputExample {

    eff InputOutput {
        pub def read(): String
        pub def write(s: String): Unit
    }

    pub def main(): Result[String, Unit] \ IO = {

        println("");
        println("First with hardcoded input:");
        try helloExample() with InputOutput {
            def read(k) = k("Alice")
            def write(s, k) = {println(s); k()}
        };

        println("");
        println("Then with actual console interaction:");
        runWithConsole(helloExample)
    }

    def runWithConsole(f: Unit -> t \ ef + InputOutput): Result[String, t] \ ef + IO = {
        try Ok(f()) with InputOutput {
            def read(k) = match Console.readLine() {
                case Err(msg) => Err(msg)
                case Ok(line) => k(line)
            }
            def write(s, k) = {Console.println(s); Console.flush(); k()}
        }
    }


    pub def helloExample(): Unit \ InputOutput = {
        do InputOutput.write("What is your name?");
        let name = do InputOutput.read;
        do InputOutput.write("Hello ${name}!")
    }

}
