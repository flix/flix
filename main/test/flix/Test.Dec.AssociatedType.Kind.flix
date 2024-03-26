mod Test.Dec.Assoc.Type.Kind {

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

    trait Foo[t] {
        type K: Type -> Type
        pub def f(x: t): Foo.K[t][String]
    }

    instance Foo[Int32] {
        type K = List
        pub def f(x: Int32): List[String] = "${x}" :: Nil
    }

    instance Foo[Int64] {
        type K = Vector
        pub def f(x: Int64): Vector[String] = Vector#{"${x}"}
    }


    ////////////////////////////////////////////////////////
    // Polymorphic functions using Foo                    //
    ////////////////////////////////////////////////////////

    pub def duplicate(x: a): (Foo.K[a][String], Foo.K[a][String]) with Foo[a] =
        (Foo.f(x), Foo.f(x))

    pub def twice(f: Foo.K[a][String] -> a, x: a): Foo.K[a][String] with Foo[a] =
        Foo.f(x)
            |> f
            |> Foo.f


    ////////////////////////////////////////////////////////
    // Monomorphic functions using Foo                    //
    ////////////////////////////////////////////////////////

    // List

    pub def duplicateInt3201(x: Int32): List[String] =
        List.append(Foo.f(x), Foo.f(x))

    pub def duplicateInt3202(x: Int32): List[String] =
        let (a, b) = duplicate(x);
        List.append(a, b)

    pub def twiceInt3201(x: Int32): List[String] =
        Foo.f(x)
            |> List.length
            |> Foo.f

    pub def twiceInt3202(x: Int32): List[String] =
        twice(List.length, x)


    // Vector

    pub def duplicateInt6401(x: Int64): Vector[String] =
        Vector.append(Foo.f(x), Foo.f(x))

    pub def duplicateInt6402(x: Int64): Vector[String] =
        let (a, b) = duplicate(x);
        Vector.append(a, b)

    pub def twiceInt6401(x: Int64): Vector[String] =
        Foo.f(x)
            |> Vector.length
            |> Int32.toInt64
            |> Foo.f

    pub def twiceInt6402(x: Int64): Vector[String] =
        twice(Vector.length >> Int32.toInt64, x)


    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////

    @Test
    pub def testInt3201(): Bool =
        List#{"1", "1"} == duplicateInt3201(1)

    @Test
    pub def testInt3202(): Bool =
        List#{"1", "1"} == duplicateInt3202(1)

    @Test
    pub def testInt3203(): Bool =
        List#{"124", "124"} == duplicateInt3201(124)

    @Test
    pub def testInt3204(): Bool =
        List#{"124", "124"} == duplicateInt3202(124)

    @Test
    pub def testInt3205(): Bool =
        List#{"1"} == twiceInt3201(5)

    @Test
    pub def testInt3206(): Bool =
        List#{"1"} == twiceInt3202(5)

    @Test
    pub def testInt3207(): Bool =
        List#{"1"} == twiceInt3201(200)

    @Test
    pub def testInt3208(): Bool =
        List#{"1"} == twiceInt3202(200)


    // Vector

    @Test
    pub def testInt6401(): Bool =
        Vector#{"1", "1"} == duplicateInt6401(1i64)

    @Test
    pub def testInt6402(): Bool =
        Vector#{"1", "1"} == duplicateInt6402(1i64)

    @Test
    pub def testInt6403(): Bool =
        Vector#{"124", "124"} == duplicateInt6401(124i64)

    @Test
    pub def testInt6404(): Bool =
        Vector#{"124", "124"} == duplicateInt6402(124i64)

    @Test
    pub def testInt6405(): Bool =
        Vector#{"1"} == twiceInt6401(5i64)

    @Test
    pub def testInt6406(): Bool =
        Vector#{"1"} == twiceInt6402(5i64)

    @Test
    pub def testInt6407(): Bool =
        Vector#{"1"} == twiceInt6401(200i64)

    @Test
    pub def testInt6408(): Bool =
        Vector#{"1"} == twiceInt6402(200i64)


}
