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
    def testRegions2RegionsNested01(): Bool & Impure =
        region _ {
            let a1 = [1; 8] @ Static;
            a1[0] == 1 and 1 ==
            region _ {
                let a2 = [1; 8] @ Static;
                a2[0]
            }
        }

    @test
    def testRegions2RegionsNested02(): Bool & Impure =
        region _ {
            let l1 = new MutList(Static);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region _ {
                let l2 = new MutList(Static);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2RegionsNested03(): Bool & Impure =
        region _ {
            let m1 = new MutMap(Static);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region _ {
                let m2 = new MutMap(Static);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }
    @test
    def testRegions2RegionsNested04(): Bool & Impure =
        region r1 {
            let a1 = [1; 8] @ r1;
            a1[0] == 1 and 1 ==
            region _ {
                let a2 = [1; 8] @ Static;
                a2[0]
            }
        }

    @test
    def testRegions2RegionsNested05(): Bool & Impure =
        region r1 {
            let l1 = new MutList(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region _ {
                let l2 = new MutList(Static);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2RegionsNested06(): Bool & Impure =
        region r1 {
            let m1 = new MutMap(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region _ {
                let m2 = new MutMap(Static);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2RegionsNested07(): Bool & Impure =
        region _ {
            let a1 = [1; 8] @ Static;
            a1[0] == 1 and 1 ==
            region r2 {
                let a2 = [1; 8] @ r2;
                a2[0]
            }
        }

    @test
    def testRegions2RegionsNested08(): Bool & Impure =
        region _ {
            let l1 = new MutList(Static);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = new MutList(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2RegionsNested09(): Bool & Impure =
        region _ {
            let m1 = new MutMap(Static);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = new MutMap(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2RegionsNested10(): Bool =
        region r1 {
            let a1 = [1; 8] @ r1;
            a1[0] == 1 and 1 ==
            region r2 {
                let a2 = [1; 8] @ r2;
                a2[0]
            }
        }

    @test
    def testRegions2RegionsNested11(): Bool =
        region r1 {
            let l1 = new MutList(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = new MutList(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2RegionsNested12(): Bool =
        region r1 {
            let m1 = new MutMap(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = new MutMap(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
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
