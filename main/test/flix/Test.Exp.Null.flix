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
