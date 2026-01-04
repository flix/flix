mod Test.Derives.Coerce {

    use Assert.assertEq;

    enum UnitEnum with Coerce {
        case UnitEnum
    }

    enum Int32Enum(Int32) with Coerce

    enum TupleEnum(Int32, String) with Coerce

    enum PolyEnum[a](a) with Coerce

    enum RegionEnum[r: Eff, a: Type](Region[r], a) with Coerce

    @Test
    def testUnitEnum(): Unit \ Assert =
        assertEq(expected = (), Coerce.coerce(UnitEnum.UnitEnum))

    @Test
    def testInt32Enum(): Unit \ Assert =
        assertEq(expected = 123, Coerce.coerce(Int32Enum.Int32Enum(123)))

    @Test
    def testTupleEnum(): Unit \ Assert =
        assertEq(expected = (123, "Hello"), Coerce.coerce(TupleEnum.TupleEnum(123, "Hello")))

    @Test
    def testPolyEnum(): Unit \ Assert =
        assertEq(expected = false, Coerce.coerce(PolyEnum.PolyEnum(false)))

    @Test
    def testRegionEnum(): Unit \ Assert = region rc {
        let (_, x) = Coerce.coerce(RegionEnum.RegionEnum(rc, 123));
        assertEq(expected = 123, x)
    }

}
