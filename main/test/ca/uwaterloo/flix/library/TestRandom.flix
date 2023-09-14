@Test
def testChoose01(): Option[Int32] \ NonDet =
    region rc {
        let rnd = Random.new();
        let arr = Array.repeat(rc, 10, 42i32);
        Random.choose(rnd, arr)
    }

@Test
def testChoose02(): Option[String] \ NonDet =
    region rc {
        let rnd = Random.new();
        let arr = Array.repeat(rc, 10, "Hello World");
        Random.choose(rnd, arr)
    }

@Test
def testChoose03(): Option[Result[Int32, Int32]] \ NonDet =
    region rc {
        let rnd = Random.new();
        let arr = Array#{Ok(123), Ok(456), Err(789)} @ rc;
        Random.choose(rnd, arr)
    }

@Test
def testNextBool(): Bool \ NonDet =
    let r = Random.newWithSeed(0i64);
    Random.nextBool(r)

@Test
def testNextFloat32(): Float32 \ NonDet =
    let r = Random.new();
    Random.nextFloat32(r)

@Test
def testNextFloat64(): Float64 \ NonDet =
    let r = Random.new();
    Random.nextFloat64(r)

@Test
def testNextGaussian(): Float64 \ NonDet =
    let r = Random.new();
    Random.nextGaussian(r)

@Test
def testNextInt32(): Int32 \ NonDet =
    let r = Random.new();
    Random.nextInt32(r)

@Test
def testNextInt64(): Int64 \ NonDet =
    let r = Random.new();
    Random.nextInt64(r)

@Test
def testNextNatWithMax(): Int32 \ NonDet =
    let r = Random.new();
    Random.nextNatWithMax(r, 100)
