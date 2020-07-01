namespace Test/Exp/Null {

    @test
    def testUnitNull01(): Unit? = null

    @test
    def testStringNull01(): String? = null

    @test
    def testRecordNull01(): {}? = null

    @test
    def testRecordNull02(): {name: String}? = null

    @test
    def testRecordNull03(): {name: String?} = {name = null}

    @test
    def testRecordNull04(): {fstName: String, lstName: String}? = null

    @test
    def testRecordNull05(): {fstName: String?, lstName: String?} = {fstName = null, lstName = null}

    @test
    def testRecordNull06(): {fstName: String, lstName: String?} = {fstName = "Lucky", lstName = null}

    @test
    def testRecordNull07(): {fstName: String?, lstName: String} = {fstName = null, lstName = "Luke"}

    @test
    def testOptionNull01(): Option[String]? = null

    @test
    def testOptionNull02(): Option[String?] = Some(null)

    @test
    def testOptionNull03(): Option[String?]? = null

    @test
    def testListNull01(): List[String]? = null

    @test
    def testListNull02(): List[String?] = Nil

    @test
    def testListNull03(): List[String?]? = null

    @test
    def testListNull04(): List[String?] = null :: Nil

    @test
    def testListNull05(): List[String?] = null :: null :: Nil

    @test
    def testResultNull01(): Result[String, String]? = null

    @test
    def testResultNull02(): Result[String, String?]? = null

    @test
    def testResultNull03(): Result[String?, String]? = null

    @test
    def testResultNull04(): Result[String?, String?]? = null

    @test
    def testResultNull05(): Result[String?, String] = Ok(null)

    @test
    def testResultNull06(): Result[String, String?] = Err(null)


    pub enum Option[t] {
        case None,
        case Some(t)
    }

    pub enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    pub enum List[t] {
        case Nil,
        case Cons(t, List[t])
    }

}
