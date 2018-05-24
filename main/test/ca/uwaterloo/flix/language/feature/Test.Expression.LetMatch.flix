type Celsius = Celsius(Int)
type Fahrenheit = Fahrenheit(Int)

@test
def testLetMatch01(): Bool =
    let (x, y) = (1, 2);
        x + 1 == y

@test
def testLetMatch02(): Bool =
    let (x, y, z) = (1, 2, 3);
        x + y == z

@test
def testLetMatch03(): Bool =
    let ((x, y), (z, w)) = ((1, 2), (3, 4));
        x + w == y + z

@test
def testLetMatch04(): Bool =
    let Celsius(x) = Celsius(42);
        x == 42

@test
def testLetMatch05(): Bool =
    let Fahrenheit(x) = Fahrenheit(42);
        x == 42

@test
def testLetMatch06(): Bool =
    let x1 = 1;
    let x2 = 1;
    let x3 = 1;
    let x4 = 1;
    let x5 = 1;
    let x6 = 1;
    let x7 = 1;
    let x8 = 1;
    let x9 = 1;
    let y1 = 1;
    let y2 = 1;
    let y3 = 1;
    let y4 = 1;
    let y5 = 1;
    let y6 = 1;
    let y7 = 1;
    let y8 = 1;
    let y9 = 1;
        x5 == y5
