mod Test.Type.Schema {

    use Assert.assertEq

    type alias Row = #(A(Int32))

    def restSchemaRow(p: #{ | #(A(Int32)) }): Vector[Int32] =
        query p select x from A(x)

    def restTypeAlias(p: #{ | Row }): Vector[Int32] =
        query p select x from A(x)

    def restExtendedTypeAlias(p: #{ B(String) | Row }): Vector[(Int32, String)] =
        query p select (x, y) from A(x), B(y)

    @CompileTest
    def testOpenSchema01(p1: #{}): #{ | r} = p1

    @CompileTest
    def testOpenSchema02(p1: #{A()}): #{A() | r} = p1

    @CompileTest
    def testOpenSchema03(p1: #{A(), B()}): #{A(), B() | r} = p1

    @CompileTest
    def testOpenSchema04(p1: #{}, p2: #{A()}): #{A() | r} = p1 <+> p2

    @CompileTest
    def testOpenSchema05(p1: #{A()}, p2: #{}): #{A() | r} = p1 <+> p2

    @CompileTest
    def testOpenSchema06(p1: #{A()}, p2: #{B()}): #{A(), B() | r} = p1 <+> p2

    @CompileTest
    def testOpenSchema07(p1: #{A()}, p2: #{B()}): #{B(), A() | r} = p1 <+> p2

    @CompileTest
    def testOpenSchema08(p1: #{A(), B()}, p2: #{B(), C()}): #{A(), B(), C() | r} = p1 <+> p2

    @CompileTest
    def testOpenSchema09(p1: #{A(), B()}, p2: #{B(), C()}): #{C(), B(), A() | r} = p1 <+> p2

    @Test
    def testSchemaRest01(): Unit \ Assert =
        assertEq(expected = Vector#{1, 2}, restSchemaRow(#{ A(1). A(2). }))

    @Test
    def testSchemaRest02(): Unit \ Assert =
        assertEq(expected = Vector#{1, 2}, restTypeAlias(#{ A(1). A(2). }))

    @Test
    def testSchemaRest03(): Unit \ Assert =
        assertEq(expected = Vector#{(1, "a")}, restExtendedTypeAlias(#{ A(1). B("a"). }))

}
