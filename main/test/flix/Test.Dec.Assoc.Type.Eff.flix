mod Test.Dec.Assoc.Type.Eff {

    eff OutInt32 {
        pub def toStream(x: Int32): Unit
    }

    eff OutString {
        pub def toStream(x: String): Unit
    }

    trait Runner[a] {
        pub type E: Eff
        pub def run(x: a): Unit \ Runner.E[a]
    }

    instance Runner[Int32] {
        pub type E = OutInt32
        pub def run(x: Int32): Unit \ OutInt32 =
            do OutInt32.toStream(x)
    }

    instance Runner[String] {
        pub type E = OutString
        pub def run(x: String): Unit \ OutString =
            do OutString.toStream(x)
    }

    instance Runner[Vector[a]] with Runner[a] {
        pub type E = Runner.E[a]
        pub def run(x: Vector[a]): Unit \ Runner.E[a] =
            foreach (a <- x) Runner.run(a)
    }
}
