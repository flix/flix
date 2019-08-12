@test
def testNextBool(): Bool =
    let r = Random.newWithSeed(0i64);
    Random.nextBool(r)

@test
def testNextFloat64(): Float64 =
    let r = Random.new();
    Random.nextFloat64(r)

