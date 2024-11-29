mod Test.Use.Op {
    pub def foo(): String = ???

    pub eff Foo {
        def foo(): String
    }

    pub def testUseOp(): String \ Foo = {
        use Foo.foo;
        foo()
    }
}
