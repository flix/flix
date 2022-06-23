namespace Test/Exp/New {

    @test
    def testNew01(): MutList[Int32, Static] & Impure =
        new MutList(Static): MutList[Int32, Static]

    @test
    def testNew02(): MutList[Int32, Static] & Impure =
        let l = new MutList(Static);
        MutList.push!(1, l);
        l

    @test
    def testNew03(): MutList[Int32, Static] & Impure =
        let l = new MutList(Static);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        l

    @test
    def testNew04(): Unit = region r {
        let _ = new MutList(r): MutList[String, r];
        ()
    }

    @test
    def testNew05(): Int32 = region r {
        let l = new MutList(r);
        MutList.push!(1, l);
        match MutList.pop!(l) {
            case Some(1) => 1
            case _       => unreachable!()
        }
    }

    @test
    def testNew06(): Int32 = region r {
        region _ {
            region _ {
                let l = new MutList(r);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }
    }

    @test
    def testNew07(): Int32 = region _ {
        region r2 {
            region _ {
                let l = new MutList(r2);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }
    }

    @test
    def testNew08(): Int32 = region _ {
        region _ {
            region r3 {
                let l = new MutList(r3);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }
    }

    @test
    def testNew09(): Int32 & Impure = region _ {
        region _ {
            region _ {
                let l = new MutList(Static);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(1) => 1
                    case _       => unreachable!()
                }
            }
        }
    }

    @test
    def testNew10(): Int32 & Impure = region r {
        region _ {
            region _ {
                let l1 = new MutList(Static);
                let l2 = new MutList(r);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }
    }

    @test
    def testNew11(): Int32 & Impure = region _ {
        region _ {
            region _ {
                let l1 = new MutList(Static);
                let l2 = new MutList(Static);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }
    }

    @test
    def testNew12(): Int32 = region r {
        region _ {
            region _ {
                let l1 = new MutList(r);
                let l2 = new MutList(r);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }
    }

    @test
    def testNew13(): Int32 = region r1 {
        let l1 = new MutList(r1);
        region r2 {
            MutList.push!(1, l1);
            region _ {
                let l2 = new MutList(r2);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case (Some(1), Some(2)) => 1 + 2
                    case _                  => unreachable!()
                }
            }
        }
    }

    @test
    def testNew14(): MutMap[Int32, Int32, Static] & Impure =
        new MutMap(Static): MutMap[Int32, Int32, Static]

    @test
    def testNew15(): MutMap[Int32, Int32, Static] & Impure =
        let m = new MutMap(Static);
        MutMap.put!(1, 1, m);
        m

    @test
    def testNew16(): MutMap[Int32, Int32, Static] & Impure =
        let m = new MutMap(Static);
        MutMap.put!(1, 1, m);
        MutMap.put!(2, 1, m);
        MutMap.put!(3, 1, m);
        m

    @test
    def testNew17(): Unit = region r {
        let _ = new MutMap(r): MutMap[Int32, String, r];
        ()
    }

    @test
    def testNew18(): Unit = region r {
        let _ = new MutMap(r): MutMap[Int32, String, r];
        let _ = new MutMap(r): MutMap[Int32, String, r];
        let _ = new MutMap(r): MutMap[Int32, String, r];
        ()
    }

    @test
    def testNew19(): Unit & Impure = region r {
        let _ = new MutMap(r): MutMap[Int32, String, r];
        let _ = new MutMap(r): MutMap[Int32, String, r];
        let _ = new MutMap(Static): MutMap[Int32, String, Static];
        ()
    }

    @test
    def testNew20(): Int32 & Impure =
        let m = new MutMap(Static);
        MutMap.put!(1, 1, m);
        match MutMap.get(1, m) {
            case Some(1) => 1
            case _       => unreachable!()
        }

    @test
    def testNew21(): Int32 = region r {
        let m = new MutMap(r);
        MutMap.put!(1, 1, m);
        match MutMap.get(1, m) {
            case Some(1) => 1
            case _       => unreachable!()
        }
    }

    @test
    def testNew22(): Int32 = region r1 {
        let m1 = new MutMap(r1): MutMap[Int32, Int32, r1];
            region r2 {
                let m2 = new MutMap(r2): MutMap[Int32, Int32, r2];
                let l = new MutList(r2): MutList[Int32, r2];
                region _ {
                    MutMap.put!(1, 1, m1);
                    MutMap.put!(2, 2, m2);
                    MutList.push!(3, l)
                };
                match MutList.pop!(l) {
                    case Some(3) => 3
                    case _       => unreachable!()
                }
            }
    }

    @test
    def testNew23(): Int32 & Impure = region r1 {
        let m1 = new MutMap(r1): MutMap[Int32, Int32, r1];
            region r2 {
                let m2 = new MutMap(Static): MutMap[Int32, Int32, Static];
                let l = new MutList(r2): MutList[Int32, r2];
                region _ {
                    MutMap.put!(1, 1, m1);
                    MutMap.put!(2, 2, m2);
                    MutList.push!(3, l)
                };
                match MutList.pop!(l) {
                    case Some(3) => 3
                    case _       => unreachable!()
                }
            }
    }

    @test
    def testNew24(): Int32 & Impure = region r1 {
        let m1 = new MutMap(r1): MutMap[Int32, Int32, r1];
            region r2 {
                let m2 = new MutMap(r1): MutMap[Int32, Int32, r1];
                let l = new MutList(r2): MutList[Int32, r2];
                region _ {
                    new MutList(Static): MutList[Unit, Static]
                };
                region _ {
                    MutMap.put!(1, 1, m1);
                    MutMap.put!(2, 2, m2);
                    MutList.push!(3, l)
                };
                match MutList.pop!(l) {
                    case Some(3) => 3
                    case _       => unreachable!()
                }
            }
    }

}
