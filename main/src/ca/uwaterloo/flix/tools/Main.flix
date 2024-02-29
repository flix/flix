def f1(): Bool = true or false

def f2(): Int32 = 3 + 2

@Test
def test01(): Bool = Assert.eq(true, f1())

@Test
def test02(): Bool = Assert.eq(5, f2())
