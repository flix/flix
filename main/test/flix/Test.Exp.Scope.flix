mod Test.Exp.Scope {

    // ensure that type variables are propagated correctly within the region
    pub def f(r: Region[r], x: Unit -> Unit \ r): Unit = ???
    pub def g(): Unit = region r {
        f(r, () -> unchecked_cast(() as _ \ r))
    }
}
