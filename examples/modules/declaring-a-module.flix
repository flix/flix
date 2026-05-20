mod Calc {
    pub def sum(x: Int32, y: Int32): Int32 = x + y
}

def main(): Unit \ IO =
    let result = Calc.sum(123, 456);
    println(result)
