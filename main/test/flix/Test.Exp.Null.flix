mod Test.Exp.Null {

    @test
    def testNull01(): Null = null

    @test
    def testNullUnit01(): Unit = unchecked_cast(null as Unit)

    @test
    def testNullString01(): String = checked_cast(null)

    @test
    def testNullArray01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Array[String, rc] \ IO)
    }

    @test
    def testNullChannel01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Receiver[String, rc] \ IO)
    }

    @test
    def testNullRecord01(): {|} = unchecked_cast(null as {|})

    @test
    def testNullRecord02(): {name = String} = unchecked_cast(null as {name = String})

    @test
    def testNullRecord03(): {name = String} = {name = checked_cast(null)}

    @test
    def testNullRecord04(): {fstName = String, lstName = String} = unchecked_cast(null as {fstName = String, lstName = String})

    @test
    def testNullRecord05(): {fstName = String, lstName = String} = {fstName = checked_cast(null), lstName = checked_cast(null)}

    @test
    def testNullRef01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Ref[String, rc] \ IO)
    }

    @test
    def testNullTuple01(): (String, String) = unchecked_cast(null as (String, String))

    @test
    def testNullTuple02(): (String, String) = (checked_cast(null), "abc")

    @test
    def testNullTuple03(): (String, String) = ("abc", checked_cast(null))

    @test
    def testNullOption01(): Option[String] = unchecked_cast(null as Option[String])

    @test
    def testNullOption02(): Option[String] = Some(checked_cast(null))

    @test
    def testNullList01(): List[String] = unchecked_cast(null as List[String])

    @test
    def testNullList02(): List[String] = (checked_cast(null)) :: Nil

    @test
    def testNullResult01(): Result[String, String] = unchecked_cast(null as Result[String, String])

    @test
    def testNullResult03(): Result[String, String] = Ok(checked_cast(null))

    @test
    def testNullResult04(): Result[String, String] = Err(checked_cast(null))

    @test
    def testNullArrow01(): (String -> String) = (_: String) -> checked_cast(null)

}
