mod Test.Exp.New {

    @test
    def testNewMutList1Region04(): Unit =
        region rc {
            discard MutList.new(rc): MutList[String, rc];
            ()
        }

    @test
    def testNewMutList1Region05(): Int32 =
        region rc {
            let l = MutList.new(rc);
            MutList.push!(1, l);
            match MutList.pop!(l) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        }

    @test
    def testNewMutList2RegionsFlat04(): Bool =
        let res1 = region rc {
            discard MutList.new(rc): MutList[String, rc];
            ()
        };
        let res2 = region rc {
            discard MutList.new(rc): MutList[Int32, rc];
            ()
        };
        res1 == () and res1 == res2

    @test
    def testNewMutList2RegionsFlat05(): Bool =
        let res1 = region rc {
            let l = MutList.new(rc);
            MutList.push!(1, l);
            match MutList.pop!(l) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        let res2 = region rc {
            let l = MutList.new(rc);
            MutList.push!(1, l);
            match MutList.pop!(l) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        res1 + res2 == 2

    @test
    def testNewMutList2RegionsNested02(): Int32 =
        region rc {
            region _ {
                let l = MutList.new(rc);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testNewMutList2RegionsNested03(): Int32 =
        region _ {
            region rc2 {
                let l = MutList.new(rc2);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testNew2MutLists2NestedRegions03(): Int32 =
        region rc {
            region _ {
                let l1 = MutList.new(rc);
                let l2 = MutList.new(rc);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNew2MutLists2NestedRegions04(): Int32 =
        region rc1 {
            region rc2 {
                let l1 = MutList.new(rc1);
                let l2 = MutList.new(rc2);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNew2MutLists2NestedRegions05(): Int32 =
        region rc1 {
            let l1 = MutList.new(rc1);
            MutList.push!(1, l1);
            region rc2 {
                let l2 = MutList.new(rc2);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNew2MutLists2NestedRegions06(): Int32 =
        region rc1 {
            let l1 = MutList.new(rc1);
            region rc2 {
                MutList.push!(1, l1);
                let l2 = MutList.new(rc2);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNewMutMap1Region04(): Unit =
        region rc {
            discard MutMap.new(rc): MutMap[Int32, String, rc];
            ()
        }

    @test
    def testNewMutMap1Region05(): Int32 =
        region rc {
            let m = MutMap.new(rc);
            MutMap.put!(1, 1, m);
            match MutMap.get(1, m) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        }

    @test
    def testNewMutMap2RegionsFlat04(): Bool =
        let res1 = region rc {
            discard MutMap.new(rc): MutMap[Int32, String, rc];
            ()
        };
        let res2 = region rc {
            discard MutMap.new(rc): MutMap[String, Int32, rc];
            ()
        };
        res1 == () and res1 == res2

    @test
    def testNewMutMap2RegionsFlat05(): Bool =
        let res1 = region rc {
            let m = MutMap.new(rc);
            MutMap.put!(1, 1, m);
            match MutMap.get(1, m) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        let res2 = region rc {
            let m = MutMap.new(rc);
            MutMap.put!(1, 1, m);
            match MutMap.get(1, m) {
                case Some(1) => 1
                case _       => unreachable!()
            }
        };
        res1 + res2 == 2

    @test
    def testNewMutMap2RegionsNested02(): Int32 =
        region rc {
            region _ {
                let m = MutMap.new(rc);
                MutMap.put!(1, 1, m);
                match MutMap.get(1, m) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testNewMutMap2RegionsNested03(): Int32 =
        region _ {
            region rc2 {
                let m = MutMap.new(rc2);
                MutMap.put!(1, 1, m);
                match MutMap.get(1, m) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testNew2MutMaps2NestedRegions03(): Int32 =
        region rc {
            region _ {
                let m1 = MutMap.new(rc);
                let m2 = MutMap.new(rc);
                MutMap.put!(1, 1, m1);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNew2MutMaps2NestedRegions04(): Int32 =
        region rc1 {
            region rc2 {
                let m1 = MutMap.new(rc1);
                let m2 = MutMap.new(rc2);
                MutMap.put!(1, 1, m1);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNew2MutMaps2NestedRegions05(): Int32 =
        region rc1 {
            let m1 = MutMap.new(rc1);
            MutMap.put!(1, 1, m1);
            region rc2 {
                let m2 = MutMap.new(rc2);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

    @test
    def testNew2MutMaps2NestedRegions06(): Int32 =
        region rc1 {
            let m1 = MutMap.new(rc1);
            region rc2 {
                MutMap.put!(1, 1, m1);
                let m2 = MutMap.new(rc2);
                MutMap.put!(2, 2, m2);
                match (MutMap.get(1, m1), MutMap.get(2, m2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }

}
