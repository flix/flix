namespace Test/Exp/Regions {

    // Uses references and regions (e.g. could just duplicate most test cases for both).
    // Test cases that use 0, 1, 2, and 3 nested regions.
    // Test cases that use 0, 1, 2, 3 data structures nested inside each other (e.g. array inside array inside array).
    // Test cases that mix both multiple regions and multiple data structures.

    @test
    def testRegions0Regions01(): Array[Int32, Static] & Impure =
        []

    @test
    def testRegions0Regions02(): MutList[Int32, Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions03(): MutMap[Int32, Int32, Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions1Region01(): Bool =
        region r {
            let a = [1; 8];
            a[0] == 1
        }

    @test
    def testRegions1Region02(): Bool =
        region r {
            let l = new MutList(r);
            MutList.push!(1, l);
            MutList.pop!(l) == Some(1)
        }

    @test
    def testRegions1Region03(): Bool =
        region r {
            let m = new MutMap(r);
            MutMap.put!(1, 1, m);
            MutMap.get(1, m) == Some(1)
        }

    @test
    def testRegions0RegionsReferences01(): Ref[Array[Int32, Static], Static] & Impure =
        ref []

    @test
    def testRegions0RegionsReferences02(): Ref[MutList[Int32, Static], Static] & Impure =
        ref new MutList(Static)

    @test
    def testRegions0RegionsReferences03(): Ref[MutMap[Int32, Int32, Static], Static] & Impure =
        ref new MutMap(Static)


}
