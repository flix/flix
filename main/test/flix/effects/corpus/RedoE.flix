eff E {
    def op(): Unit
}

def handleE(f: Unit -> Unit \ ef): Unit \ ef = {
    run f() with handler E {
        def op(_k) = E.op()
    }
}