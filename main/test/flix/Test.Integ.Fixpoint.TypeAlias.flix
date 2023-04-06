mod Test.Integ.Fixpoint.TypeAlias {
    type alias X = List[Int32]

    pub def f(x: X): Unit = let _x = inject x into X; ()
}
