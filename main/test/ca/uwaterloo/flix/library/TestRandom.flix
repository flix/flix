@test
def testChoose01(): Option[Int32] \ NonDet =
    region rc {
        let rnd = Random.new();
        let arr = Array.repeat(rc, 10, 42i32);
        Random.choose(rnd, arr)
    }

@test
def testChoose02(): Option[String] \ NonDet =
    region rc {
        let rnd = Random.new();
        let arr = Array.repeat(rc, 10, "Hello World");
        Random.choose(rnd, arr)
    }

@test
def testChoose03(): Option[Result[Int32, Int32]] \ NonDet =
    region rc {
        let rnd = Random.new();
        let arr = Array#{Ok(123), Ok(456), Err(789)} @ rc;
        Random.choose(rnd, arr)
    }

@test
def testNextBool(): Bool \ NonDet =
    let r = Random.newWithSeed(0i64);
    Random.nextBool(r)

@test
def testNextFloat32(): Float32 \ NonDet =
    let r = Random.new();
    Random.nextFloat32(r)

@test
def testNextFloat64(): Float64 \ NonDet =
    let r = Random.new();
    Random.nextFloat64(r)

@test
def testNextGaussian(): Float64 \ NonDet =
    let r = Random.new();
    Random.nextGaussian(r)

@test
def testNextInt32(): Int32 \ NonDet =
    let r = Random.new();
    Random.nextInt32(r)

@test
def testNextInt64(): Int64 \ NonDet =
    let r = Random.new();
    Random.nextInt64(r)

@test
def testNextNatWithMax(): Int32 \ NonDet =
    let r = Random.new();
    Random.nextNatWithMax(r, 100)
