eff Con {
    pub def read(): Unit
    pub def print(s: String): Unit
}

def main(): Int32 =
    //let _ = f();
    try {
        u()
    } with Con {
        def read() = resume(())
        def print(_) = 42
    }

def f(): Int32 = g() + 1

def g(): Int32 = h(1)

def h(x: Int32): Int32 = x + 1

def u(): Int32 \ Con =
    let name = v();
    let greetings = "Hello ${name}";
    do Con.print(greetings);
    String.length("${name}")

def v(): String \ Con = do Con.read()
