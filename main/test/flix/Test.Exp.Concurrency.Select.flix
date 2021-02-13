namespace Test/Exp/Concurrency/Select {

    @test
    def testSelectBuffered01(): Bool & Impure =
        let c1 = chan Int 1;
        spawn c1 <- 1;
        select {
            case x <- c1 => x == 1
        }

    @test
    def testSelectBuffered02(): Bool & Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        select {
            case x <- c1 => x == 1
            case x <- c2 => x == 2
        }

    @test
    def testSelectBuffered03(): Bool & Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        spawn c3 <- 3;
        select {
            case x <- c1 => x == 1
            case x <- c2 => x == 2
            case x <- c3 => x == 3
        }

    @test
    def testSelectBuffered04(): Bool & Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        let c4 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        spawn c3 <- 3;
        spawn c4 <- 4;
        select {
            case x <- c1 => x == 1
            case x <- c1 => x == 1
            case x <- c2 => x == 2
            case x <- c2 => x == 2
            case x <- c3 => x == 3
            case x <- c3 => x == 3
            case x <- c4 => x == 4
            case x <- c4 => x == 4
        }

    @test
    def testSelectBuffered05(): Bool & Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        let c4 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        spawn c3 <- 3;
        spawn c4 <- 4;
        select {
            case x <- c4 => x == 4
            case x <- c3 => x == 3
            case x <- c2 => x == 2
            case x <- c1 => x == 1
            case x <- c4 => x == 4
            case x <- c3 => x == 3
            case x <- c2 => x == 2
            case x <- c1 => x == 1
        }

    @test
    def testSelectBuffered06(): Bool & Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        let c4 = chan Int 1;
        spawn c2 <- 1;
        select {
            case _ <- c4 => false
            case _ <- c3 => false
            case x <- c2 => x == 1
            case _ <- c1 => false
        }

    @test
    def testSelectBuffered07(): Bool & Impure =
        let c1 = chan Int8 1;
        let c2 = chan Int16 1;
        let c3 = chan Int32 1;
        let c4 = chan Int64 1;
        spawn c1 <- 1i8;
        spawn c2 <- 2i16;
        spawn c3 <- 3i32;
        spawn c4 <- 4i64;
        select {
            case x <- c4 => x == 4i64
            case x <- c3 => x == 3i32
            case x <- c2 => x == 2i16
            case x <- c1 => x == 1i8
        }

    @test
    def testSelectDefault01(): Bool & Impure =
        select {
            case x <- chan Bool 1 => x
            case _                => true
        }

    @test
    def testSelectDefault02(): Bool & Impure =
        (1 + select {
            case x <- chan Int64 1 => 2
            case _                 => 1
        }) == 2

    @test
    def testSelectRandom01(): Unit & Impure = {
        let c9 = chan Unit 0;
        let c10 = chan Unit 0;
        let c11 = chan Unit 0;
        let c12 = chan Unit 0;
        let c13 = chan Unit 0;
        spawn { c13 <- () ; () } ; spawn { c12 <- () ; () } ; spawn { c11 <- () ; () } ; spawn { c10 <- () ; () } ; spawn { c9 <- () ; () } ; () ; select {
        case _ <- c13 => select {
        case _ <- c11 => ()
        case _ <- c11 => ()
        } ; <- c9 ; <- c10 ; <- c12 ; let c42 = chan Unit 0;
        let c43 = chan Unit 0;
        let c44 = chan Unit 0;
        let c45 = chan Unit 0;
        spawn { c45 <- () ; () } ; spawn { c44 <- () ; () } ; spawn { c43 <- () ; () } ; spawn { c42 <- () ; () } ; () ; select {
        case _ <- c43 => <- c42 ; <- c44 ; <- c45
        case _ <- c42 => <- c45 ; <- c43 ; <- c44 ; ()
        }
        case _ <- c13 => select {
        case _ <- c11 => ()
        case _ <- c11 => ()
        } ; <- c9 ; <- c10 ; <- c12 ; let c42 = chan Unit 0;
        let c43 = chan Unit 0;
        let c44 = chan Unit 0;
        let c45 = chan Unit 0;
        spawn { c45 <- () ; () } ; spawn { c44 <- () ; () } ; spawn { c43 <- () ; () } ; spawn { c42 <- () ; () } ; () ; select {
        case _ <- c43 => <- c42 ; <- c44 ; <- c45 ; ()
        case _ <- c42 => <- c45 ; <- c43 ; <- c44
        }
        };
        ()
    }

    /*@test
    def testSelectRandom02(): Unit & Impure = {
        let c10 = chan Unit 0;
        let c11 = chan Unit 0;
        let c12 = chan Unit 0;
        let c13 = chan Unit 0;
        spawn { c13 <- () ; () } ; spawn { c12 <- () ; () } ; if (false) { spawn { c11 <- () ; () } ; spawn { c10 <- () ; () } ; if (true) { () } else { () } } else { spawn { c11 <- () ; () } ; spawn { c10 <- () ; () } ; if (true) { () } else { () } } ; select {
        case _ <- c10 => <- c13 ; <- c12 ; <- c11
        case _ <- c13 => <- c12 ; <- c10 ; <- c11 ; ()
        case _ <- c11 => <- c13 ; <- c12 ; <- c10 ; ()
        };
        ()
    }*/

    @test
    def testSelectRandom03(): Unit & Impure = {
        let c14 = chan Unit 0;
        let c15 = chan Unit 0;
        let c16 = chan Unit 0;
        let c17 = chan Unit 0;
        let c18 = chan Unit 0;
        spawn { c18 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; () ; spawn { c18 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; () ; let c141 = chan Unit 0;
        let c139 = chan Unit 0;
        let c140 = chan Unit 0;
        spawn { c141 <- () ; () } ; spawn { c140 <- () ; () } ; spawn { c139 <- () ; () } ; () ; spawn { c141 <- () ; () } ; spawn { c140 <- () ; () } ; spawn { c139 <- () ; () } ; () ; spawn { c141 <- () ; () } ; spawn { c140 <- () ; () } ; spawn { c139 <- () ; () } ; () ; spawn { select {
        case _ <- c141 => <- c140 ; <- c139 ; ()
        } } ; spawn { select {
        case _ <- c139 => <- c140 ; <- c141 ; ()
        case _ <- c140 => <- c141 ; <- c139 ; ()
        } } ; spawn { select {
        case _ <- c139 => select {
        case _ <- c140 => ()
        case _ <- c140 => ()
        } ; <- c141 ; ()
        case _ <- c141 => <- c140 ; <- c139 ; ()
        } } ; () ; spawn { select {
        case _ <- c15 => <- c18 ; <- c16 ; <- c17 ; <- c14 ; ()
        case _ <- c14 => <- c18 ; <- c16 ; <- c15 ; <- c17 ; ()
        case _ <- c17 => <- c15 ; <- c18 ; select {
        case _ <- c16 => ()
        case _ <- c16 => ()
        } ; <- c14
        case _ <- c18 => <- c15 ; <- c14 ; <- c17 ; <- c16 ; ()
        } } ; spawn { select {
        case _ <- c16 => <- c18 ; <- c17 ; <- c15 ; <- c14 ; ()
        } } ; ();
        ()
    }

    @test
    def testSelectRandom04(): Unit & Impure = {
        let c10 = chan Unit 0;
        let c11 = chan Unit 0;
        let c12 = chan Unit 0;
        spawn { c12 <- () ; () } ; spawn { c11 <- () ; () } ; spawn { c10 <- () ; () } ; let c40 = chan Unit 0;
        let c41 = chan Unit 0;
        let c42 = chan Unit 0;
        spawn { c42 <- () ; () } ; spawn { c41 <- () ; () } ; spawn { c40 <- () ; () } ; () ; spawn { c42 <- () ; () } ; spawn { c41 <- () ; () } ; spawn { c40 <- () ; () } ; spawn { c42 <- () ; () } ; spawn { c41 <- () ; () } ; spawn { c40 <- () ; () } ; () ; spawn { c42 <- () ; () } ; spawn { c41 <- () ; () } ; spawn { c40 <- () ; () } ; () ; spawn { select {
        case _ <- c40 => <- c41 ; <- c42 ; ()
        } } ; spawn { select {
        case _ <- c40 => <- c42 ; <- c41 ; ()
        case _ <- c40 => <- c42 ; <- c41 ; ()
        } } ; spawn { select {
        case _ <- c42 => <- c40 ; <- c41 ; ()
        } } ; spawn { select {
        case _ <- c42 => <- c40 ; <- c41 ; ()
        case _ <- c42 => <- c40 ; <- c41 ; ()
        } } ; () ; select {
        case _ <- c10 => <- c12 ; <- c11 ; ()
        };
        ()
    }

    @test
    def testSelectRandom05(): Unit & Impure = {
        let c14 = chan Unit 0;
        let c15 = chan Unit 0;
        let c16 = chan Unit 0;
        let c17 = chan Unit 0;
        spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; () ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; () ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; () ; () ; spawn { select {
        case _ <- c17 => <- c14 ; <- c15 ; <- c16
        } } ; spawn { select {
        case _ <- c16 => <- c14 ; <- c15 ; <- c17 ; ()
        } } ; spawn { select {
        case _ <- c17 => <- c15 ; <- c16 ; <- c14 ; let c171 = chan Unit 0;
        let c172 = chan Unit 0;
        let c173 = chan Unit 0;
        spawn { c173 <- () ; () } ; spawn { c172 <- () ; () } ; spawn { c171 <- () ; () } ; () ; spawn { c173 <- () ; () } ; spawn { c172 <- () ; () } ; spawn { c171 <- () ; () } ; () ; spawn { c173 <- () ; () } ; spawn { c172 <- () ; () } ; spawn { c171 <- () ; () } ; () ; spawn { c173 <- () ; () } ; spawn { c172 <- () ; () } ; spawn { c171 <- () ; () } ; () ; () ; spawn { select {
        case _ <- c172 => <- c173 ; <- c171 ; ()
        } } ; spawn { select {
        case _ <- c171 => <- c173 ; <- c172 ; ()
        case _ <- c173 => <- c172 ; <- c171 ; ()
        } } ; spawn { select {
        case _ <- c171 => <- c173 ; <- c172 ; ()
        case _ <- c173 => <- c171 ; <- c172 ; ()
        case _ <- c171 => <- c173 ; <- c172 ; ()
        } } ; spawn { select {
        case _ <- c171 => <- c173 ; select {
        case _ <- c172 => ()
        case _ <- c172 => ()
        } ; ()
        case _ <- c173 => <- c171 ; <- c172 ; ()
        } } ; ()
        case _ <- c14 => <- c17 ; <- c15 ; <- c16 ; ()
        } } ; spawn { select {
        case _ <- c17 => <- c16 ; <- c15 ; <- c14 ; ()
        } } ; ();
        ()
    }

    @test
    def testSelectRandom06(): Unit & Impure = {
        let c2 = chan Unit 0;
        let c3 = chan Unit 0;
        spawn { c3 <- () ; () ; () } ; spawn { c2 <- () ; () ; () } ; () ; <- c3 ; select {
        case _ <- c2 => ()
        case _ <- c2 => ()
        } ; let c24 = chan Unit 0;
        spawn { select {
        case _ <- c24 => ()
        case _ <- c24 => ()
        } } ; c24 <- () ; ();
        ()
    }

    @test
    def testSelectRandom07(): Unit & Impure = {
        let c12 = chan Unit 0;
        let c13 = chan Unit 0;
        let c14 = chan Unit 0;
        let c15 = chan Unit 0;
        let c16 = chan Unit 0;
        spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; spawn { c13 <- () ; () } ; spawn { c12 <- () ; () } ; () ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; spawn { c13 <- () ; () } ; spawn { c12 <- () ; () } ; () ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; spawn { c14 <- () ; () } ; spawn { c13 <- () ; () } ; spawn { c12 <- () ; () } ; () ; () ; spawn { select {
        case _ <- c14 => <- c12 ; <- c13 ; <- c15 ; <- c16 ; ()
        } } ; spawn { select {
        case _ <- c16 => <- c13 ; <- c14 ; <- c12 ; <- c15 ; ()
        case _ <- c15 => <- c14 ; <- c13 ; <- c16 ; <- c12 ; ()
        case _ <- c12 => <- c13 ; <- c15 ; <- c16 ; <- c14 ; let c180 = chan Unit 0;
        let c178 = chan Unit 0;
        let c179 = chan Unit 0;
        spawn { c180 <- () ; () } ; spawn { c179 <- () ; () } ; spawn { c178 <- () ; () } ; () ; spawn { c180 <- () ; () } ; spawn { c179 <- () ; () } ; spawn { c178 <- () ; () } ; () ; () ; spawn { select {
        case _ <- c178 => <- c180 ; <- c179
        case _ <- c179 => select {
        case _ <- c178 => ()
        case _ <- c178 => ()
        } ; <- c180 ; ()
        } } ; spawn { select {
        case _ <- c178 => <- c179 ; <- c180 ; ()
        case _ <- c180 => <- c178 ; <- c179 ; ()
        } } ; ()
        } } ; spawn { select {
        case _ <- c14 => <- c15 ; <- c12 ; <- c16 ; <- c13 ; ()
        case _ <- c13 => <- c12 ; <- c14 ; <- c15 ; <- c16 ; ()
        } } ; ();
        ()
    }

    @test
    def testSelectRandom08(): Unit & Impure = {
        let c15 = chan Unit 0;
        let c16 = chan Unit 0;
        let c17 = chan Unit 0;
        let c18 = chan Unit 0;
        let c19 = chan Unit 0;
        spawn { c19 <- () ; () } ; spawn { c18 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; () ; spawn { c19 <- () ; () } ; spawn { c18 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; () ; spawn { c19 <- () ; () } ; spawn { c18 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; () ; spawn { c19 <- () ; () } ; spawn { c18 <- () ; () } ; spawn { c17 <- () ; () } ; spawn { c16 <- () ; () } ; spawn { c15 <- () ; () } ; () ; () ; spawn { select {
        case _ <- c15 => <- c17 ; <- c18 ; <- c16 ; <- c19 ; ()
        } } ; spawn { select {
        case _ <- c15 => <- c19 ; <- c18 ; <- c16 ; <- c17 ; ()
        case _ <- c18 => <- c19 ; <- c15 ; <- c16 ; <- c17 ; ()
        case _ <- c19 => <- c16 ; <- c17 ; <- c18 ; <- c15 ; ()
        case _ <- c17 => <- c15 ; <- c18 ; <- c16 ; <- c19 ; ()
        } } ; spawn { select {
        case _ <- c17 => <- c18 ; <- c15 ; <- c16 ; <- c19 ; ()
        case _ <- c15 => <- c17 ; <- c16 ; <- c18 ; <- c19 ; ()
        case _ <- c16 => <- c15 ; <- c18 ; <- c17 ; <- c19 ; ()
        case _ <- c19 => <- c15 ; <- c18 ; <- c16 ; <- c17 ; ()
        } } ; spawn { select {
        case _ <- c17 => <- c18 ; <- c16 ; <- c19 ; <- c15 ; ()
        case _ <- c16 => <- c18 ; select {
        case _ <- c15 => ()
        case _ <- c15 => ()
        } ; <- c17 ; <- c19 ; let c214 = chan Unit 0;
        let c215 = chan Unit 0;
        let c216 = chan Unit 0;
        let c217 = chan Unit 0;
        spawn { c217 <- () ; () } ; spawn { c216 <- () ; () } ; spawn { c215 <- () ; () } ; spawn { c214 <- () ; () } ; () ; select {
        case _ <- c215 => <- c214 ; <- c216 ; <- c217 ; ()
        case _ <- c214 => <- c216 ; <- c215 ; <- c217 ; ()
        case _ <- c216 => <- c214 ; <- c217 ; <- c215
        }
        } } ; ();
        ()
    }

    @test
    def testSelectRandom09(): Unit & Impure = {
        let c6 = chan Unit 0;
        let c7 = chan Unit 0;
        let c8 = chan Unit 0;
        let c9 = chan Unit 0;
        spawn { select {
        case _ <- c8 => ()
        case _ <- c8 => ()
        } ; c9 <- () ; () } ; spawn { <- c7 ; c8 <- () ; () ; () } ; spawn { select {
        case _ <- c6 => ()
        case _ <- c6 => ()
        } ; c7 <- () ; () ; () } ; () ; c6 <- () ; () ; <- c9;
        ()
    }

    @test
    def testSelectRandom10(): Unit & Impure = {
        let c10 = chan Unit 0;
        let c11 = chan Unit 0;
        let c12 = chan Unit 0;
        let c13 = chan Unit 0;
        spawn { c13 <- () ; () } ;
        spawn { c12 <- () ; () } ;
        spawn { c11 <- () ; () } ;
        spawn { c10 <- () ; () } ;
        select {
            case _ <- c10 => <- c13 ; <- c12 ; <- c11
            case _ <- c13 => <- c12 ; <- c10 ; <- c11
            case _ <- c11 => <- c13 ; <- c12 ; <- c10
        };
        ()
    }

}
