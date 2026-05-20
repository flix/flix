mod Test.Type.Schema {

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

}
