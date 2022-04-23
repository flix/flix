namespace Test/Dec/Effect {
    eff Console {
        // Will be removed after Weeder checks are added
        def readLine(): String
        def println(s: String): Unit
    }

    eff Throw {
        // Will be removed after Weeder checks are added
        def throw(): a
    }

    eff Nondeterminism {
        def fail(): Unit
        // Will be removed after Weeder checks are added
        def decide(): Bool
    }

    eff State {
        // Will be removed after Weeder checks are added
        def get_(): Int
        def set_(x: Int): Unit
    }

    eff Threading {
        def yield(): Unit
        def spawn_(f: Unit -> Unit): Unit

        // Will be removed after Weeder checks are added
        def getNext(): Option[Unit -> Unit]
        def enqueue(f: Unit -> Unit): Unit
    }

    eff Empty

    eff EmptyWithParens {

    }

    // Will be removed after Weeder checks are added
    eff Polymorphic[a] {
        def op[a](): a
    }
}
