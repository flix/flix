mod Test.Dec.Effect {
    pub eff Console {
        def println(s: String): Unit
    }

    pub eff Fail {
        def fail(): Unit
    }

    pub eff State {
        def set_(x: Int32): Unit
    }

    pub eff Threading {
        def yield_(): Unit
        def spawn_(f: Unit -> Unit): Unit
    }

    pub eff Empty

    pub eff EmptyWithParens {

    }
}
