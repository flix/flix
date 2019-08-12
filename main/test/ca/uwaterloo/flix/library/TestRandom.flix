@test
def testNextBool(): Bool =
    let r = Random.newWithSeed(0i64);
    Random.nextBool(r)

@test
def testNextFloat32(): Float32 =
    let r = Random.new();
    Random.nextFloat32(r)

@test
def testNextFloat64(): Float64 =
    let r = Random.new();
    Random.nextFloat64(r)

@test
def testNextGaussian(): Float64 =
    let r = Random.new();
    Random.nextGaussian(r)

@test
def testNextInt32(): Int32 =
    let r = Random.new();
    Random.nextInt32(r)

@test
def testNextInt64(): Int64 =
    let r = Random.new();
    Random.nextInt64(r)
