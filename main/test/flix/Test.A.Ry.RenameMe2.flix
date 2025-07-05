trait A[a] {
    type Typ
    pub def foo(x: a): A.Typ[a]
}

enum CharAdapter[a, t](a, t -> Char)

instance A[CharAdapter[a, t]] with A[a] where A.Typ[a] ~ t {
    type Typ = Char
    pub def foo(adapter: CharAdapter[a, t]): Char =
        let CharAdapter.CharAdapter(a, f) = adapter;
        f(A.foo(a))
}

enum Int8Adapter[a, t](a, t -> Int8)

instance A[Int8Adapter[a, t]] with A[a] where A.Typ[a] ~ t {
    type Typ = Int8
    pub def foo(adapter: Int8Adapter[a, A.Typ[a]]): Int8 =
        let Int8Adapter.Int8Adapter(a, f) = adapter;
        f(A.foo(a))
}
