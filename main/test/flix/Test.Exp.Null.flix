mod Test.Exp.Null {

    @CompileTest
    def testNull01(): Null = null

    @CompileTest
    def testNullUnit01(): Unit = unchecked_cast(null as Unit)

    @CompileTest
    def testNullString01(): String = checked_cast(null)

    @CompileTest
    def testNullArray01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Array[String, rc] \ IO)
    }

    @CompileTest
    def testNullChannel01(): Unit \ IO = {
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Receiver[String] \ IO)
    }

    @CompileTest
    def testNullRecord01(): {|} = unchecked_cast(null as {|})

    @CompileTest
    def testNullRecord02(): {name = String} = unchecked_cast(null as {name = String})

    @CompileTest
    def testNullRecord03(): {name = String} = {name = checked_cast(null)}

    @CompileTest
    def testNullRecord04(): {fstName = String, lstName = String} = unchecked_cast(null as {fstName = String, lstName = String})

    @CompileTest
    def testNullRecord05(): {fstName = String, lstName = String} = {fstName = checked_cast(null), lstName = checked_cast(null)}

    @CompileTest
    def testNullRef01(): Unit \ IO = region rc {
        let _ = rc;
        discard unchecked_cast(unchecked_cast(null as _ \ IO) as Ref[String, rc] \ IO)
    }

    @CompileTest
    def testNullTuple01(): (String, String) = unchecked_cast(null as (String, String))

    @CompileTest
    def testNullTuple02(): (String, String) = (checked_cast(null), "abc")

    @CompileTest
    def testNullTuple03(): (String, String) = ("abc", checked_cast(null))

    @CompileTest
    def testNullOption01(): Option[String] = unchecked_cast(null as Option[String])

    @CompileTest
    def testNullOption02(): Option[String] = Some(checked_cast(null))

    @CompileTest
    def testNullList01(): List[String] = unchecked_cast(null as List[String])

    @CompileTest
    def testNullList02(): List[String] = (checked_cast(null)) :: Nil

    @CompileTest
    def testNullResult01(): Result[String, String] = unchecked_cast(null as Result[String, String])

    @CompileTest
    def testNullResult03(): Result[String, String] = Ok(checked_cast(null))

    @CompileTest
    def testNullResult04(): Result[String, String] = Err(checked_cast(null))

    @CompileTest
    def testNullArrow01(): (String -> String) = (_: String) -> checked_cast(null)

}
