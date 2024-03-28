def f1(): Int32 = Int32.bitwiseNot(6)

def f2(): Int32 = -f1()

def f3(): Int32 = -6

def f4(): Int32 = -f3()

@Test
def test1(): Bool = Assert.eq(-7, f1())

@Test
def test2(): Bool = Assert.eq(7, f2())

@Test
def test3(): Bool = Assert.eq(-6, f3())

@Test
def test4(): Bool = Assert.eq(6, f4())
