def foo(): Unit =
    try {
        f()
    } with {
        case ex: Ex => ()
    }

def f(): Int32 = 123
