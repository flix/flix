def f1(): Int32 = 4 + 2

def f2(): Int32 = 4 - 2

def f3(): Int32 = 4 * 2

def f4(): Int32 = 4 / 2

def f5(): Int32 = Int32.modulo(4, 2)

def f6(): Int32 = Int32.remainder(4, 2)

def f7(): Int32 = Int32.bitwiseNot(4)

def f8(): Int32 = Int32.bitwiseAnd(4, 2)

def f9(): Int32 = Int32.bitwiseOr(4, 2)

def f10(): Int32 = Int32.bitwiseXor(4, 2)

def f11(): Int32 = Int32.leftShift(4, 2)

def f12(): Int32 = Int32.rightShift(4, 2)

def f13(): Bool = 4 == 2

def f131(): Bool = Eq.eq(4, 2)

def f14(): Bool = 4 != 2

def f141(): Bool = Eq.neq(4, 2)

def f15(): Bool = 4 < 2

def f151(): Bool = Order.less(4, 2)

def f16(): Bool = 4 <= 2

def f161(): Bool = Order.lessEqual(4, 2)

def f17(): Bool = 4 >= 2

def f171(): Bool = Order.greaterEqual(4, 2)

def f18(): Bool = 4 > 2

def f181(): Bool = Order.greater(4, 2)

@Test
def test01(): Bool = Assert.eq(6, f1())

@Test
def test02(): Bool = Assert.eq(2, f2())

@Test
def test03(): Bool = Assert.eq(8, f3())

@Test
def test04(): Bool = Assert.eq(2, f4())

@Test
def test05(): Bool = Assert.eq(0, f5())

@Test
def test06(): Bool = Assert.eq(0, f6())

@Test
def test07(): Bool = Assert.eq(-5, f7())

@Test
def test08(): Bool = Assert.eq(0, f8())

@Test
def test09(): Bool = Assert.eq(6, f9())

@Test
def test10(): Bool = Assert.eq(6, f10())

@Test
def test11(): Bool = Assert.eq(16, f11())

@Test
def test12(): Bool = Assert.eq(1, f12())

@Test
def test13(): Bool = Assert.eq(false, f13())

@Test
def test131(): Bool = Assert.eq(false, f131())

@Test
def test14(): Bool = Assert.eq(true, f14())

@Test
def test141(): Bool = Assert.eq(true, f141())

@Test
def test15(): Bool = Assert.eq(false, f15())

@Test
def test151(): Bool = Assert.eq(false, f151())

@Test
def test16(): Bool = Assert.eq(false, f16())

@Test
def test161(): Bool = Assert.eq(false, f161())

@Test
def test17(): Bool = Assert.eq(true, f17())

@Test
def test171(): Bool = Assert.eq(true, f171())

@Test
def test18(): Bool = Assert.eq(true, f18())

@Test
def test181(): Bool = Assert.eq(true, f181())
