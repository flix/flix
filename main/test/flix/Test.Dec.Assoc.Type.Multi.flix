mod Test.Dec.Assoc.Type.Multi {

    use Assert.assertEq

    ////////////////////////////////////////////////////////
    // Extra parameter in trailing position                //
    ////////////////////////////////////////////////////////

    trait Build[a] {
        type Builder[a, r: Region]: Type
        pub def build(b: Build.Builder[a, r]): a \ r
    }

    instance Build[List[t]] {
        type Builder[List[t], r] = MutList[t, r]
        pub def build(b: MutList[t, r]): List[t] \ r = MutList.toList(b)
    }

    pub def buildList(rc: Region[r]): List[Int32] \ r =
        let ml = MutList.empty(rc);
        MutList.push(1, ml);
        MutList.push(2, ml);
        Build.build(ml)

    ////////////////////////////////////////////////////////
    // Extra parameter reordered                          //
    ////////////////////////////////////////////////////////

    // The point-free encoding cannot express this: the extra parameter is
    // not the trailing parameter of the right-hand side.
    trait Reorder[a] {
        type Swapped[a, b]: Type
        pub def swap(x: a, y: Reorder.Swapped[a, b]): b
    }

    instance Reorder[Int32] {
        type Swapped[Int32, b] = (b, Int32)
        pub def swap(_x: Int32, y: (b, Int32)): b = fst(y)
    }

    ////////////////////////////////////////////////////////
    // Extra parameter discarded                          //
    ////////////////////////////////////////////////////////

    // The point-free encoding cannot express this either: the extra
    // parameter is unused on the right-hand side.
    trait Ignore[a] {
        type Const[a, b]: Type
        pub def wrap(x: a, y: b): Ignore.Const[a, b]
    }

    instance Ignore[Int32] {
        type Const[Int32, b] = Int32
        pub def wrap(x: Int32, _y: b): Int32 = x
    }

    ////////////////////////////////////////////////////////
    // Extra parameter used twice                         //
    ////////////////////////////////////////////////////////

    trait Twice[a] {
        type Pair[a, b]: Type
        pub def dup(x: a, y: b): Twice.Pair[a, b]
    }

    instance Twice[Int32] {
        type Pair[Int32, b] = (b, b)
        pub def dup(_x: Int32, y: b): (b, b) = (y, y)
    }

    ////////////////////////////////////////////////////////
    // Effect-kinded associated type with extra parameter  //
    ////////////////////////////////////////////////////////

    trait Runner[a] {
        type Aef[a, r: Region]: Eff
        pub def exec(x: a, rc: Region[r]): Int32 \ Runner.Aef[a, r]
    }

    instance Runner[Int32] {
        type Aef[Int32, r] = r
        pub def exec(x: Int32, _rc: Region[r]): Int32 \ r = x
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
        assertEq(expected = "a", Reorder.swap(1, ("a", 2)))

    @Test
    pub def testIgnore01(): Unit \ Assert =
        assertEq(expected = 42, Ignore.wrap(42, "ignored"))

    @Test
    pub def testTwice01(): Unit \ Assert =
        assertEq(expected = ("x", "x"), Twice.dup(1, "x"))

    @Test
    pub def testRunner01(): Unit \ Assert = region rc {
        assertEq(expected = 7, Runner.exec(7, rc))
    }

}
