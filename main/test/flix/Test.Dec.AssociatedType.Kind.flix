mod Test.Dec.AssociatedTypes.Kind {

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


}
