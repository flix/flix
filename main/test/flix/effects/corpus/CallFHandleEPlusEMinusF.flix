eff E {
    def op(): Unit
}

eff F {
    def op(): Unit
}

def handleE(f: Unit -> Unit \ ef + F): Unit \ ef + E - F = {
    run f() with handler E {
        def op(_k) = ()
    }
}
