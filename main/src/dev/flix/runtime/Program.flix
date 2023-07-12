eff Con {
    pub def read(): Unit
    pub def print(s: String): Unit
}

def main(): Unit =
    let _ = f();
    try {
        u()
    } with Con {
        def read() = resume(())
        def print(_) = resume(())
    };
    ()

def f(): Int32 = g() + 1

def g(): Int32 = h(1)

def h(x: Int32): Int32 = x + 1

def u(): Unit \ Con =
    let name = do Con.read();
    let greetings = "Hello ${name}";
    do Con.print(greetings)
