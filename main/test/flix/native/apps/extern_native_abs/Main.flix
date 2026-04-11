extern native(symbol = "abs")
def cAbs(x: Int32): Int32

pub def main(): Unit \ IO =
    if (cAbs(-42) == 42)
        println("extern-native: ok")
    else
        println("extern-native: bad")
