def f1(): Bool = false

def f2(a: Int32, b: Int32): Int32 = a + b

def f3(n: Int64): Bool = n >= 18i64

@Test
def test01(): Bool = Assert.eq(false, f1())

@Test
def test02(): Bool = Assert.eq(5, f2(3, 2))

@Test
def test03(): Bool = Assert.eq(true, f3(18i64))
