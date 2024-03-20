def f1(k: Int32): Int32 \ IO =
    let a = 10;
    let b = 20;
    println("(a + b) * k");
    (a + b) * k

def f2(): Int32 \ IO = f1(3)

@Test
def test1(): Bool\ IO = Assert.eq(30, f1(1))

@Test
def test2(): Bool\ IO = Assert.eq(60, f1(2))

@Test
def test3(): Bool\ IO = Assert.eq(90, f2())
