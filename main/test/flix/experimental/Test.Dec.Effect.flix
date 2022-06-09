namespace Test/Dec/Effect {
    eff Console {
        pub def println(s: String): Unit
    }

    eff Fail {
        pub def fail(): Unit
    }

    eff State {
        pub def set_(x: Int32): Unit
    }

    eff Threading {
        pub def yield(): Unit
        pub def spawn_(f: Unit -> Unit): Unit
    }

    eff Empty

    eff EmptyWithParens {

    }
}
