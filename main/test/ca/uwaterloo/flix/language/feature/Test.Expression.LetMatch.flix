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
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let x = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let y = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
    let _ = 1;
        x == y
