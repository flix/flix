@test
def testOneOf01(): Option[Int32] \ IO =
    region rc {
        let rnd = Random.fresh();
        let arr = Array.repeat(rc, 10, 42i32);
        Random.oneOf(rnd, arr)
    }

@test
def testOneOf02(): Option[String] \ IO =
    region rc {
        let rnd = Random.fresh();
        let arr = Array.repeat(rc, 10, "Hello World");
        Random.oneOf(rnd, arr)
    }

@test
def testOneOf03(): Option[Result[Int32, Int32]] \ IO =
    region rc {
        let rnd = Random.fresh();
        let arr = Array#{Ok(123), Ok(456), Err(789)} @ rc;
        Random.oneOf(rnd, arr)
    }

@test
def testNextBool(): Bool \ IO =
    let r = Random.newWithSeed(0i64);
    Random.nextBool(r)

@test
def testNextFloat32(): Float32 \ IO =
    let r = Random.fresh();
    Random.nextFloat32(r)

@test
def testNextFloat64(): Float64 \ IO =
    let r = Random.fresh();
    Random.nextFloat64(r)

@test
def testNextGaussian(): Float64 \ IO =
    let r = Random.fresh();
    Random.nextGaussian(r)

@test
def testNextInt32(): Int32 \ IO =
    let r = Random.fresh();
    Random.nextInt32(r)

@test
def testNextInt64(): Int64 \ IO =
    let r = Random.fresh();
    Random.nextInt64(r)

@test
def testNextNatWithMax(): Int32 \ IO =
    let r = Random.fresh();
    Random.nextNatWithMax(r, 100)
