eff E {
    def op(): Unit
}

def foo(): Unit  = E.op() without E
