@test
def testChoose01(): Bool & Impure =
    let ran = Random.new();
    let num = 42;
    let a = [num; 10];
    Random.choose(ran, a) == Some(num)

@test
def testChoose02(): Bool & Impure =
    let ran = Random.new();
    let str = "Hello World";
    let a = [str; 10];
    Random.choose(ran, a) == Some(str)

@test
def testChoose03(): Bool =
    use Random.{choose, newWithSeed};
    region r {
        let a = [Ok(123), Ok(456), Err(789)];
        let test1 = choose(newWithSeed(r, 42i64), a) == Some(Err(789));
        let test2 = choose(newWithSeed(r, 54i64), a) == Some(Ok(123));
        test1 and test2
    }

@test
def testNextBool(): Bool =
    region r {
        let ran = Random.newWithSeed(r, 0i64);
        Random.nextBool(ran)
    }

@test
def testNextFloat32(): Float32 & Impure =
    let ran = Random.new();
    Random.nextFloat32(ran)

@test
def testNextFloat64(): Float64 & Impure =
    let ran = Random.new();
    Random.nextFloat64(ran)

@test
def testNextGaussian(): Float64 & Impure =
    let ran = Random.new();
    Random.nextGaussian(ran)

@test
def testNextInt32(): Int32 & Impure =
    let ran = Random.new();
    Random.nextInt32(ran)

@test
def testNextInt64(): Int64 & Impure =
    let ran = Random.new();
    Random.nextInt64(ran)

@test
def testNextNatWithMax(): Bool & Impure =
    let ran = Random.new();
    let n = Random.nextNatWithMax(ran, 100);
    0 <= n and n < 100

@test
def testShuffleList(): Bool & Impure =
    let ran = Random.new();
    let list = 1 :: 2 :: 3 :: 4 :: Nil;
    let shuffledList = Random.shuffleList(ran, list);
    List.sum(shuffledList) == 10 and List.length(shuffledList) == 4

@test
def testShuffleArray(): Unit =
    let ran = Random.new();
    let arr = [1, 2, 3, 4, Nil] @ static;
    Random.shuffleArray(ran, arr);
    List.sum(shuffledList) == 10 and List.length(shuffledList) == 4
