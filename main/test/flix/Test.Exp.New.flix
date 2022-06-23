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
            case Some(x) => x
            case None    => unreachable!()
        }
    }

    @test
    def testNew06(): Int32 = region r {
        region _ {
            region _ {
                let l = new MutList(r);
                MutList.push!(1, l);
                match MutList.pop!(l) {
                    case Some(x) => x
                    case None    => unreachable!()
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
                    case Some(x) => x
                    case None    => unreachable!()
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
                    case Some(x) => x
                    case None    => unreachable!()
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
                    case Some(x) => x
                    case None    => unreachable!()
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
                    case Some(1, 2) => 1 + 2
                    case None       => unreachable!()
                }
            }
        }
    }

    @test
    def testNew11(): Int32 & Impure = region r {
        region _ {
            region _ {
                let l1 = new MutList(Static);
                let l2 = new MutList(Static);
                MutList.push!(1, l1);
                MutList.push!(2, l2);
                match (MutList.pop!(l1), MutList.pop!(l2)) {
                    case Some(1, 2) => 1 + 2
                    case None       => unreachable!()
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
                    case Some(1, 2) => 1 + 2
                    case None       => unreachable!()
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
                    case Some(1, 2) => 1 + 2
                    case None       => unreachable!()
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
        MutMap.put!(1, 1, l);
        m

}
