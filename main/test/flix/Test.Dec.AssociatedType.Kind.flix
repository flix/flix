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
        type K = Set
        pub def f(x: Int64): Set[String] = Set#{"${x}"}
    }


    ////////////////////////////////////////////////////////
    // Polymorphic functions using Foo                    //
    ////////////////////////////////////////////////////////

    pub def duplicate(x: a): (Foo.K[a][String], Foo.K[a][String]) with Foo[a] =
        (Foo.f(x), Foo.f(x))


    ////////////////////////////////////////////////////////
    // Monomorphic functions using Foo                    //
    ////////////////////////////////////////////////////////

    pub def twiceInt3201(x: Int32): List[String] =
        List.append(Foo.f(x), Foo.f(x))

    pub def twiceInt3202(x: Int32): List[String] =
        let (a, b) = duplicate(x);
        List.append(a, b)

    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////

    @Test
    pub def testInt3201(): Bool =
        List#{"1", "1"} == twiceInt3201(1)

    @Test
    pub def testInt3202(): Bool =
        List#{"1", "1"} == twiceInt3202(1)

}
