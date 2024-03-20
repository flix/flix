def f1(): (Bool, Int16) = (true, 1661i16)

def f2(): ((Int8, Int32, String), Float64, Int32, Bool, (Bool, Int16)) =
    ((111i8, 333, "Hello "), 222.0, 5 + 8, 7 > 7, f1())

@Test
def test1(): Bool = Assert.eq((true, 1661i16), f1())

@Test
def test2(): Bool = Assert.eq(((111i8, 333, "Hello "), 222.0, 13, false, (true, 1661i16)), f2())
