mod Test.Exp.NewRegion {

    @test
    def testEmptyMutList1Region04(): Unit =
        region rc {
            discard (MutList.empty(rc): MutList[String, rc]);
            ()
        }

    @test
    def testEmptyMutList1Region05(): Int32 =
        region rc {
            let l = MutList.empty(rc);
            MutList.push!(1, l);
            match MutList.pop!(l) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        }

    @test
    def testEmptyMutList2RegionsFlat04(): Bool =
        let res1 = region rc {
            discard (MutList.empty(rc): MutList[String, rc]);
            ()
        };
        let res2 = region rc {
            discard (MutList.empty(rc): MutList[Int32, rc]);
            ()
        };
        res1 == () and res1 == res2

    @test
    def testEmptyMutList2RegionsFlat05(): Bool =
        let res1 = region rc {
            let l = MutList.empty(rc);
            MutList.push!(1, l);
            match MutList.pop!(l) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        let res2 = region rc {
            let l = MutList.empty(rc);
            MutList.push!(1, l);
            match MutList.pop!(l) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        res1 + res2 == 2

    @test
    def testEmptyMutList2RegionsNested02(): Int32 =
        region rc {
            region _ {
                let l = MutList.empty(rc);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testEmptyMutList2RegionsNested03(): Int32 =
        region _ {
            region rc2 {
                let l = MutList.empty(rc2);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutLists2NestedRegions03(): Int32 =
        region rc {
            region _ {
                let l1 = MutList.empty(rc);
                let l2 = MutList.empty(rc);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutLists2NestedRegions04(): Int32 =
        region rc1 {
            region rc2 {
                let l1 = MutList.empty(rc1);
                let l2 = MutList.empty(rc2);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutLists2NestedRegions05(): Int32 =
        region rc1 {
            let l1 = MutList.empty(rc1);
            MutList.push!(1, l1);
            region rc2 {
                let l2 = MutList.empty(rc2);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutLists2NestedRegions06(): Int32 =
        region rc1 {
            let l1 = MutList.empty(rc1);
            region rc2 {
                MutList.push!(1, l1);
                let l2 = MutList.empty(rc2);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmptyMutMap1Region04(): Unit =
        region rc {
            discard (MutMap.empty(rc): MutMap[Int32, String, rc]);
            ()
        }

    @test
    def testEmptyMutMap1Region05(): Int32 =
        region rc {
            let m = MutMap.empty(rc);
            MutMap.put!(1, 1, m);
            match MutMap.get(1, m) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        }

    @test
    def testEmptyMutMap2RegionsFlat04(): Bool =
        let res1 = region rc {
            discard (MutMap.empty(rc): MutMap[Int32, String, rc]);
            ()
        };
        let res2 = region rc {
            discard (MutMap.empty(rc): MutMap[String, Int32, rc]);
            ()
        };
        res1 == () and res1 == res2

    @test
    def testEmptyMutMap2RegionsFlat05(): Bool =
        let res1 = region rc {
            let m = MutMap.empty(rc);
            MutMap.put!(1, 1, m);
            match MutMap.get(1, m) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        let res2 = region rc {
            let m = MutMap.empty(rc);
            MutMap.put!(1, 1, m);
            match MutMap.get(1, m) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        res1 + res2 == 2

    @test
    def testEmptyMutMap2RegionsNested02(): Int32 =
        region rc {
            region _ {
                let m = MutMap.empty(rc);
                MutMap.put!(1, 1, m);
                match MutMap.get(1, m) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testEmptyMutMap2RegionsNested03(): Int32 =
        region _ {
            region rc2 {
                let m = MutMap.empty(rc2);
                MutMap.put!(1, 1, m);
                match MutMap.get(1, m) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutMaps2NestedRegions03(): Int32 =
        region rc {
            region _ {
                let m1 = MutMap.empty(rc);
                let m2 = MutMap.empty(rc);
                MutMap.put!(1, 1, m1);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutMaps2NestedRegions04(): Int32 =
        region rc1 {
            region rc2 {
                let m1 = MutMap.empty(rc1);
                let m2 = MutMap.empty(rc2);
                MutMap.put!(1, 1, m1);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutMaps2NestedRegions05(): Int32 =
        region rc1 {
            let m1 = MutMap.empty(rc1);
            MutMap.put!(1, 1, m1);
            region rc2 {
                let m2 = MutMap.empty(rc2);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testEmpty2MutMaps2NestedRegions06(): Int32 =
        region rc1 {
            let m1 = MutMap.empty(rc1);
            region rc2 {
                MutMap.put!(1, 1, m1);
                let m2 = MutMap.empty(rc2);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

}
