mod Test.Exp.Null {

    @Test
    def testNull01(): Null = null

    @Test
    def testNullUnit01(): Unit = unchecked_cast(null as Unit)

    @Test
    def testNullString01(): String = unchecked_cast(null as String)

    @Test
    def testNullArray01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Array[String, rc] \ IO)
    }

    @Test
    def testNullChannel01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Receiver[String, rc] \ IO)
    }

    @Test
    def testNullRecord01(): {} = unchecked_cast(null as {})

    @Test
    def testNullRecord02(): {name = String} = unchecked_cast(null as {name = String})

    @Test
    def testNullRecord03(): {name = String} = {name = unchecked_cast(null as String)}

    @Test
    def testNullRecord04(): {fstName = String, lstName = String} = unchecked_cast(null as {fstName = String, lstName = String})

    @Test
    def testNullRecord05(): {fstName = String, lstName = String} = {fstName = unchecked_cast(null as String), lstName = unchecked_cast(null as String)}

    @Test
    def testNullRef01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Ref[String, rc] \ IO)
    }

    @Test
    def testNullTuple01(): (String, String) = unchecked_cast(null as (String, String))

    @Test
    def testNullTuple02(): (String, String) = (unchecked_cast(null as String), "abc")

    @Test
    def testNullTuple03(): (String, String) = ("abc", unchecked_cast(null as String))

    @Test
    def testNullOption01(): Option[String] = unchecked_cast(null as Option[String])

    @Test
    def testNullOption02(): Option[String] = Some(unchecked_cast(null as String))

    @Test
    def testNullList01(): List[String] = unchecked_cast(null as List[String])

    @Test
    def testNullList02(): List[String] = (unchecked_cast(null as String)) :: Nil

    @Test
    def testNullResult01(): Result[String, String] = unchecked_cast(null as Result[String, String])

    @Test
    def testNullResult03(): Result[String, String] = Ok(unchecked_cast(null as String))

    @Test
    def testNullResult04(): Result[String, String] = Err(unchecked_cast(null as String))

    @Test
    def testNullArrow01(): (String -> String) = (_: String) -> unchecked_cast(null as String)

}
