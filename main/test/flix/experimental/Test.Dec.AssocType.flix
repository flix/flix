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
}
