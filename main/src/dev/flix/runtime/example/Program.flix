eff Console {
    pub def read(): Int32
    pub def print(s: String): Unit
}

def main(): Int32 =
    //let _ = f();
    try {
        u()
    } with Console {
        def read() = resume(42)
        def print(s) = ___PRINT___(s); resume() // actually print to terminal
    }

def f(): Int32 = g() + 1

def g(): Int32 = h(1)

def h(x: Int32): Int32 = x + 1

def u(): Int32 \ Console =
    let name = v();
    let greetings = "Hello ${name}";
    do Console.print(greetings);
    String.length("${name}")

def v(): String \ Console = do Console.read()
