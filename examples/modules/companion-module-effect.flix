eff StdOut {
    def printStr(s: String): Unit
}

mod StdOut {
    pub def println(x: a): Unit \ StdOut with ToString[a] =
        StdOut.printStr("${x}")
}

def main(): Unit \ IO =
    run {
        StdOut.println(42)
    } with StdOut {
        def printStr(s, k) = {println(s); k()}
    }
