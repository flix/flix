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

}
