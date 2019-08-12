@test
def testChoose01(): Int32 =
    let r = Random.newWithSeed(0i64);
    let a = [42i32; 10];
    Random.choose(r, a)

@test
def testChoose02(): Str =
    let r = Random.newWithSeed(0i64);
    let a = ["Hello World"; 10];
    Random.choose(r, a)

@test
def testChoose03(): Option[Int] =
    let r = Random.newWithSeed(0i64);
    let a = [Some(42); 10];
    Random.choose(r, a)

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
