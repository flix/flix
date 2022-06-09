namespace Test/Exp/Scope {

    // ensure that type variables are propagated correctly within the region
    def f(r: Region[r], x: Unit -> Unit \ { Write(r) }): Unit = ???
    def g(): Unit = region r {
        f(r, () -> () as \ { Write(r) })
    }
}
