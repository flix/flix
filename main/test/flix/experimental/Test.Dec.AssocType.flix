mod Test.Dec.AssocType {

    class Coll[a] {
        type Elem[a]: Type

        pub def contains(x: Coll.Elem[a], c: a): Bool

        pub def head(x: a): Option[Coll.Elem[a]]
    }

    instance Coll[String] {
        type Elem[String] = Char

        pub def contains(x: Char, s: String): Bool = s |> String.toList |> List.memberOf(x)

        pub def head(x: String): Option[Char] = if (String.isEmpty(x)) None else Some(String.charAt(0, x))
    }

    pub def hasA(s: String): Bool = Coll.contains('A', s)

    pub def hasAGeneric(s: a): Bool with Coll[a] where Coll.Elem[a] ~ Char = Coll.contains('A', s)

    @test
    def testHasA01(): Bool = hasA("ABC")

    @test
    def testHasA02(): Bool = not hasA("XYZ")

    @test
    def testHasAGeneric01(): Bool = hasAGeneric("ABC")

    @test
    def testHasAGeneric02(): Bool = not hasAGeneric("XYZ")

    @test
    def testHead01(): Bool = Coll.head("ABC") == Some('A')

    def testRecordHelper01(): {x = Coll.Elem[String]} = {x = 'a'}

    @test
    def testRecord01(): Bool =  testRecordHelper01().x == 'a'

    class Iterable2[a] {
        type Eff[a]: Bool
        type Elem[a]: Type

        pub def iterator(rh: Region[r], x: a): Iterator[Iterable2.Elem[a], Iterable2.Eff[a] and r, r] & Iterable2.Eff[a] and r
    }

    instance Iterable2[String] {
        type Eff[String] = Pure
        type Elem[String] = Char

        pub def iterator(rh: Region[r], x: String): Iterator[Char, r, r] \ r = String.iterator(rh, x)
    }

    instance Iterable2[MutList[a, r]] {
        type Eff[MutList[a, r]] = r
        type Elem[MutList[a, r]] = a

        pub def iterator(rh: Region[r2], x: MutList[a, r]): Iterator[a, r and r2, r2] & r and r2 = MutList.iterator(rh, x)
    }

}
