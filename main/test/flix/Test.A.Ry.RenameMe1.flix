trait A[a] {
    type Typ
    pub def foo(x: a): A.Typ[a]
}

enum Adapter[t, a, b](t, a -> b)

instance A[Adapter[t, a, b]] with A[t] where A.Typ[t] ~ a {
    type Typ = b
    pub def foo(adapter: Adapter[t, a, b]): b =
        let Adapter.Adapter(x, f) = adapter;
        f(A.foo(x))
}
