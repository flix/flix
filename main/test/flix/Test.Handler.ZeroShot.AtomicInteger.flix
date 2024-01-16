mod Test.Handler.ZeroShot.AtomicInteger {

    enum AtomicInt(##java.util.concurrent.atomic.AtomicInteger)

    instance Eq[AtomicInt] {
        pub def eq(a: AtomicInt, b: AtomicInt): Bool = {
            import java.util.concurrent.atomic.AtomicInteger.intValue(): Int32 \ {} as intValue;
            match (a, b) {
                case (AtomicInt.AtomicInt(objA), AtomicInt.AtomicInt(objB)) => intValue(objA) == intValue(objB)
            }
        }
    }

    instance ToString[AtomicInt] {
        pub def toString(x: AtomicInt): String = {
            import java.util.concurrent.atomic.AtomicInteger.intValue(): Int32 \ {} as intValue;
            match x {
                case AtomicInt.AtomicInt(obj) => ToString.toString(intValue(obj))
            }
        }
    }

    eff Throw {
        pub def throw(): AtomicInt
    }

    @Test
    def testLinear01(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        def f1() = do Throw.throw();
        let result = try {
            f1()
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testLinear02(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        let result = try {
            f2()
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testLinear03(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        def f1() = do Throw.throw();
        def f2() = { f1(); unreachable!() };
        def f3() = { f2(); unreachable!() };
        let result = try {
            f3()
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testRecursiveLetRec01(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        let result = try {
            r(0)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testRecursiveLetRec02(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        let result = try {
            r(1)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testRecursiveLetRec03(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        def f() = do Throw.throw();
        def r(x) = if (x == 0) f() else { r(x - 1); unreachable!() } ;
        let result = try {
            r(10)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testMutualRecursive01(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        let result = try {
            mutual1(0)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testMutualRecursive02(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        let result = try {
            mutual1(1)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testMutualRecursive03(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        let result = try {
            mutual1(2)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testMutualRecursive04(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        let result = try {
            mutual1(3)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testMutualRecursive05(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        let result = try {
            mutual1(10)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(42))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(42)), result)

    @Test
    def testMutualRecursive06(): Bool =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): ##java.util.concurrent.atomic.AtomicInteger \ {} as newAtomicInteger;
        let result = try {
            mutual1(10)
        } with Throw {
            def throw(_k) = AtomicInt.AtomicInt(newAtomicInteger(40))
        };
        Assert.eq(AtomicInt.AtomicInt(newAtomicInteger(40)), result)

    def f(): AtomicInt \ Throw =
         do Throw.throw();
         unreachable!()

    def mutual1(x: Int32): AtomicInt \ Throw =
        if (x == 0) f() else { mutual2(x - 1); unreachable!() }

    def mutual2(x: Int32): AtomicInt \ Throw =
        if (x == 0) f() else { mutual1(x - 1); unreachable!() }

}
