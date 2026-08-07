mod Test.Dec.Assoc.Type.Multi {

    use Assert.assertEq

    ////////////////////////////////////////////////////////
    // Extra parameter in trailing position                //
    ////////////////////////////////////////////////////////

    trait Build[a] {
        type Builder[a, r]: Type
        pub def build(b: Build.Builder[a, r]): a \ r
    }

    instance Build[List[Int32]] {
        type Builder[List[Int32], r] = MutList[Int32, r]
        pub def build(b: MutList[Int32, r]): List[Int32] \ r = MutList.toList(b)
    }

    pub def buildList(rc: Region[r]): List[Int32] \ r =
        let ml = MutList.empty(rc);
        MutList.push(1, ml);
        MutList.push(2, ml);
        Build.build(ml)

    ////////////////////////////////////////////////////////
    // Extra parameter reordered                          //
    ////////////////////////////////////////////////////////

    // The point-free encoding cannot express this: `r` is not the trailing
    // parameter of the right-hand side.
    trait Reorder[a] {
        type Swapped[a, b]: Type
        pub def swap(x: Reorder.Swapped[a, b]): b
    }

    instance Reorder[Int32] {
        type Swapped[Int32, b] = (b, Int32)
        pub def swap(x: (b, Int32)): b = fst(x)
    }

    ////////////////////////////////////////////////////////
    // Extra parameter discarded                          //
    ////////////////////////////////////////////////////////

    // The point-free encoding cannot express this either: `b` is unused.
    trait Ignore[a] {
        type Const[a, b]: Type
        pub def wrap(x: a): Ignore.Const[a, b]
    }

    instance Ignore[Int32] {
        type Const[Int32, b] = Int32
        pub def wrap(x: Int32): Int32 = x
    }

    ////////////////////////////////////////////////////////
    // Extra parameter used twice                         //
    ////////////////////////////////////////////////////////

    trait Twice[a] {
        type Pair[a, b]: Type
        pub def dup(x: b): Twice.Pair[a, b]
    }

    instance Twice[Int32] {
        type Pair[Int32, b] = (b, b)
        pub def dup(x: b): (b, b) = (x, x)
    }

    ////////////////////////////////////////////////////////
    // Effect-kinded associated type with extra parameter  //
    ////////////////////////////////////////////////////////

    trait Runner[a] {
        type Aef[a, r]: Eff
        pub def run(x: a, rc: Region[r]): Int32 \ Runner.Aef[a, r]
    }

    instance Runner[Int32] {
        type Aef[Int32, r] = r
        pub def run(x: Int32, _rc: Region[r]): Int32 \ r = x
    }

    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////

    @Test
    pub def testBuild01(): Unit \ Assert = region rc {
        assertEq(expected = List#{1, 2}, buildList(rc))
    }

    @Test
    pub def testReorder01(): Unit \ Assert =
        assertEq(expected = "a", Reorder.swap(("a", 1)))

    @Test
    pub def testIgnore01(): Unit \ Assert =
        assertEq(expected = 42, (Ignore.wrap(42): Ignore.Const[Int32, String]))

    @Test
    pub def testTwice01(): Unit \ Assert =
        assertEq(expected = ("x", "x"), (Twice.dup("x"): Twice.Pair[Int32, String]))

    @Test
    pub def testRunner01(): Unit \ Assert = region rc {
        assertEq(expected = 7, Runner.run(7, rc))
    }

}
