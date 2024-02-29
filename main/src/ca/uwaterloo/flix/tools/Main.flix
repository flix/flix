def f1(): Int32 = 4 + 2

def f2(): Int32 = 4 - 2

def f3(): Int32 = 4 * 2

def f4(): Int32 = 4 / 2

@Test
def test01(): Bool = Assert.eq(6, f1())

@Test
def test02(): Bool = Assert.eq(2, f2())

@Test
def test03(): Bool = Assert.eq(8, f3())

@Test
def test04(): Bool = Assert.eq(2, f4())
