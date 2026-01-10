mod Test.Exp.Match.Unit {

    use Assert.{assertEq, success}

    @Test
    def matchUnit01(): Unit \ Assert = match () {
        case () => success("matched unit")
    }

    @Test
    def matchUnitVar01(): Unit \ Assert = match () {
        case x => assertEq(expected = (), x)
    }

}
