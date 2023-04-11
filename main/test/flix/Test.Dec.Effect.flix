mod Test.Dec.Effect {
    pub eff Console {
        pub def println(s: String): Unit
    }

    pub eff Fail {
        pub def fail(): Unit
    }

    pub eff State {
        pub def set_(x: Int32): Unit
    }

    pub eff Threading {
        pub def yield(): Unit
        pub def spawn_(f: Unit -> Unit): Unit
    }

    pub eff Empty

    pub eff EmptyWithParens {

    }
}
