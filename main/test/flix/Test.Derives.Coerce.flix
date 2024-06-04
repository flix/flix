mod Test.Derives.Coerce {
    enum UnitEnum with Coerce {
        case UnitEnum
    }

    enum Int32Enum(Int32) with Coerce

    enum TupleEnum(Int32, String) with Coerce

    enum PolyEnum[a](a) with Coerce

    enum RegionEnum[r: Eff, a: Type](Region[r], a) with Coerce

    @Test
    def testUnitEnum(): Bool = Coerce.coerce(UnitEnum.UnitEnum()) == ()

    @Test
    def testInt32Enum(): Bool = Coerce.coerce(Int32Enum.Int32Enum(123)) == 123

    @Test
    def testTupleEnum(): Bool = Coerce.coerce(TupleEnum.TupleEnum(123, "Hello")) == (123, "Hello")

    @Test
    def testPolyEnum(): Bool = Coerce.coerce(PolyEnum.PolyEnum(false)) == false

    @Test
    def testRegionEnum(): Bool = region rc {
        let (_, x) = Coerce.coerce(RegionEnum.RegionEnum(rc, 123));
        x == 123
    }
}
