def f1(x: Int32, y: Int32): Int32 = if (x == 5) {
    if (y == 7) {
        123
    } else {
        231
    }
} else {
    312
}

@Test
def test11(): Bool = Assert.eq(231 , f1(5, 5))

@Test
def test12(): Bool = Assert.eq(123 , f1(5, 7))

@Test
def test13(): Bool = Assert.eq(312 , f1(7, 5))

@Test
def test14(): Bool = Assert.eq(312 , f1(7, 7))
