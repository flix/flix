namespace Test/Dec/Effect {
    eff Console {
        // Will be removed after Weeder checks are added
        pub def readLine(): String
        pub def println(s: String): Unit
    }

    eff Throw {
        // Will be removed after Weeder checks are added
        pub def throw(): a
    }

    eff Nondeterminism {
        pub def fail(): Unit
        // Will be removed after Weeder checks are added
        pub def decide(): Bool
    }

    eff State {
        // Will be removed after Weeder checks are added
        pub def get_(): Int
        pub def set_(x: Int): Unit
    }

    eff Threading {
        pub def yield(): Unit
        pub def spawn_(f: Unit -> Unit): Unit

        // Will be removed after Weeder checks are added
        pub def getNext(): Option[Unit -> Unit]
        pub def enqueue(f: Unit -> Unit): Unit
    }

    eff Empty

    eff EmptyWithParens {

    }
}
