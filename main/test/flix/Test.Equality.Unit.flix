mod Test.Equality.Unit {

    use Assert.assertEq

    @Test
    def testEqUnit01(): Unit \ Assert =
        assertEq(expected = (), ())

}
