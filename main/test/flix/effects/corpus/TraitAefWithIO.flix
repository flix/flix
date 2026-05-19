trait T[t] {
    type Aef: Eff
    pub def f(x: t): Unit \ T.Aef[t]
}

instance T[Int32] {
    type Aef = {}
    pub def f(_: Int32): Unit =
        println("Hello")
}
