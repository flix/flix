eff Console {
    pub def read(): String
    pub def print(s: String): Unit
}

def main(): Int32 =
    try {
        u()
    } with Console {
        def read() = resume("John")
        def print(s) = ___PRINT___(s); resume() // actually print to terminal
    }

def u(): Int32 \ Console =
    let name = v();
    let greetings = "Hello ${name}";
    do Console.print(greetings);
    String.length("${name}")

def v(): String \ Console = do Console.read()
