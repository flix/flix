///namespace Test/Equality/Channel {
///
///    @test
///    def testEqualityChannel01(): Bool & Impure =
///        let c = chan Bool 0;
///        c == c
///
///    @test
///    def testEqualityChannel02(): Bool & Impure =
///        let c = chan Int 0;
///        c == c
///
///    @test
///    def testEqualityChannel03(): Bool & Impure =
///        let c = chan String 0;
///        c == c
///
///    @test
///    def testEqualityChannel04(): Bool & Impure =
///        let x = chan Bool 0;
///        let y = chan Bool 0;
///        x != y
///
///    @test
///    def testEqualityChannel05(): Bool & Impure =
///        let x = chan Int 0;
///        let y = chan Int 0;
///        x != y
///
///    @test
///    def testEqualityChannel06(): Bool & Impure =
///        let x = chan String 0;
///        let y = chan String 0;
///        x != y
///
///}
