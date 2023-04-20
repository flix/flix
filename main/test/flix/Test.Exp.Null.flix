mod Test.Exp.Null {

    @test
    def testNull01(): Null = null

    @test
    def testNullUnit01(): Unit = unchecked_cast(null as Unit)

    @test
    def testNullString01(): String = unchecked_cast(null as String)

    @test
    def testNullArray01(): Unit \ IO = region r {
        let _ = r;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Array[String, r] \ IO)
    }

    @test
    def testNullChannel01(): Unit \ IO = region r {
        let _ = r;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Receiver[String, r] \ IO)
    }

    @test
    def testNullRecord01(): {} = unchecked_cast(null as {})

    @test
    def testNullRecord02(): {name = String} = unchecked_cast(null as {name = String})

    @test
    def testNullRecord03(): {name = String} = {name = unchecked_cast(null as String)}

    @test
    def testNullRecord04(): {fstName = String, lstName = String} = unchecked_cast(null as {fstName = String, lstName = String})

    @test
    def testNullRecord05(): {fstName = String, lstName = String} = {fstName = unchecked_cast(null as String), lstName = unchecked_cast(null as String)}

    @test
    def testNullRef01(): Unit \ IO = region r {
        let _ = r;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Ref[String, r] \ IO)
    }

    @test
    def testNullTuple01(): (String, String) = unchecked_cast(null as (String, String))

    @test
    def testNullTuple02(): (String, String) = (unchecked_cast(null as String), "abc")

    @test
    def testNullTuple03(): (String, String) = ("abc", unchecked_cast(null as String))

    @test
    def testNullOption01(): Option[String] = unchecked_cast(null as Option[String])

    @test
    def testNullOption02(): Option[String] = Some(unchecked_cast(null as String))

    @test
    def testNullList01(): List[String] = unchecked_cast(null as List[String])

    @test
    def testNullList02(): List[String] = (unchecked_cast(null as String)) :: Nil

    @test
    def testNullResult01(): Result[String, String] = unchecked_cast(null as Result[String, String])

    @test
    def testNullResult03(): Result[String, String] = Ok(unchecked_cast(null as String))

    @test
    def testNullResult04(): Result[String, String] = Err(unchecked_cast(null as String))

    @test
    def testNullArrow01(): (String -> String) = (_: String) -> unchecked_cast(null as String)

}
