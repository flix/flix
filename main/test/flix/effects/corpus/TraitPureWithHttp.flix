trait T[t] {
    pub def f(x: t): Unit
}

instance T[Int32] {
    pub def f(_: Int32): Unit =
        let _ = Net.Http.get("http://example.com");
        ()
}
