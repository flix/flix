mod Test.Exp.Scope {

    // ensure that type variables are propagated correctly within the region
    pub def f(rc: Region[r], x: Unit -> Unit \ r): Unit = ???
    pub def g(): Unit = region rc {
        f(rc, () -> unchecked_cast(() as _ \ rc))
    }
}
