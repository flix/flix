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

}
