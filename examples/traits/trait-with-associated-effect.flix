trait Dividable[t] {
    type Aef: Eff = {}
    pub def div(x: t, y: t): t \ Dividable.Aef[t]
}

instance Dividable[Float64] {
    pub def div(x: Float64, y: Float64): Float64 = x / y
}

pub eff DivByZero {
    def raise(): Void
}

instance Dividable[Int32] {
    type Aef = DivByZero
    pub def div(x: Int32, y: Int32): Int32 \ DivByZero =
        if (y == 0) DivByZero.raise() else x / y
}

def main(): Unit \ IO =
    let x = Dividable.div(42.0, 0.0);
    let y = run { Dividable.div(42, 0) } with DivByZero { def raise(_) = -1 };
    println("x = ${x}");
    println("x = ${y}")
