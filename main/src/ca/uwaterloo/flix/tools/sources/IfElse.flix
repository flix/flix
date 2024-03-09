def f1(x: Int32): Int32 = if (x == 5) {
    123
} else {
    321
}

@Test
def test11(): Bool = Assert.eq(123 , f1(5))

@Test
def test12(): Bool = Assert.eq(321 , f1(6))
