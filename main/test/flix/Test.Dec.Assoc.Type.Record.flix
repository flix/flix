mod Test.Dec.Assoc.Type.Record {
    trait R[t] {
        type K: Type
        pub def f(x: t): R.K[t]
    }

    instance R[Int64] {
        type K = { x = Int64 }
        pub def f(x: Int64): { x = Int64 } = { x = x }
    }

    instance R[Int32] {
        type K = { x = Int64, x = Int32 }
        pub def f(x: Int32): { x = Int64, x = Int32 } = { x = Int32.toInt64(x), x = x }
    }

    instance R[Vector[a]] with R[a] {
        type K = { x = Int32, y = Vector[R.K[a]] }
        pub def f(x: Vector[a]): { x = Int32, y = Vector[R.K[a]] } = { x = Vector.length(x), y = Vector.map(R.f, x) }
    }
}
