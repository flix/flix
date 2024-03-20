def f1(): String = "hello, " + "world!"

def f2(): Int32 = 2 + 3

def f3(): BigInt = Int32.toBigInt(2) + Int32.toBigInt(3)

@Test
def test01(): Bool = Assert.eq("hello, world!", f1())

@Test
def test02(): Bool = Assert.eq(5, f2())
