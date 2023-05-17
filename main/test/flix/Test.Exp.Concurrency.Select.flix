mod Test.Exp.Concurrency.Select {

    @test
    def testSelectBuffered01(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx1) @ r;
        select {
            case x <- recv(rx1) => x == 1
        }
    }

    @test
    def testSelectBuffered02(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx1) @ r;
        spawn Channel.send(2, tx2) @ r;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
        }
    }

    @test
    def testSelectBuffered03(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        let (tx3, rx3) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx1) @ r;
        spawn Channel.send(2, tx2) @ r;
        spawn Channel.send(3, tx3) @ r;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
            case x <- recv(rx3) => x == 3
        }
    }

    @test
    def testSelectBuffered04(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        let (tx3, rx3) = Channel.buffered(r, 1);
        let (tx4, rx4) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx1) @ r;
        spawn Channel.send(2, tx2) @ r;
        spawn Channel.send(3, tx3) @ r;
        spawn Channel.send(4, tx4) @ r;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
            case x <- recv(rx2) => x == 2
            case x <- recv(rx3) => x == 3
            case x <- recv(rx3) => x == 3
            case x <- recv(rx4) => x == 4
            case x <- recv(rx4) => x == 4
        }
    }

    @test
    def testSelectBuffered05(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        let (tx3, rx3) = Channel.buffered(r, 1);
        let (tx4, rx4) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx1) @ r;
        spawn Channel.send(2, tx2) @ r;
        spawn Channel.send(3, tx3) @ r;
        spawn Channel.send(4, tx4) @ r;
        select {
            case x <- recv(rx4) => x == 4
            case x <- recv(rx3) => x == 3
            case x <- recv(rx2) => x == 2
            case x <- recv(rx1) => x == 1
            case x <- recv(rx4) => x == 4
            case x <- recv(rx3) => x == 3
            case x <- recv(rx2) => x == 2
            case x <- recv(rx1) => x == 1
        }
    }

    @test
    def testSelectBuffered06(): Bool \ IO = region r {
        let (_, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        let (_, rx3) = Channel.buffered(r, 1);
        let (_, rx4) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx2) @ r;
        select {
            case _ <- recv(rx4) => false
            case _ <- recv(rx3) => false
            case x <- recv(rx2) => x == 1
            case _ <- recv(rx1) => false
        }
    }

    @test
    def testSelectBuffered07(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        let (tx3, rx3) = Channel.buffered(r, 1);
        let (tx4, rx4) = Channel.buffered(r, 1);
        spawn Channel.send(1i8, tx1) @ r;
        spawn Channel.send(2i16, tx2) @ r;
        spawn Channel.send(3i32, tx3) @ r;
        spawn Channel.send(4i64, tx4) @ r;
        select {
            case x <- recv(rx4) => x == 4i64
            case x <- recv(rx3) => x == 3i32
            case x <- recv(rx2) => x == 2i16
            case x <- recv(rx1) => x == 1i8
        }
    }

    @test
    def testSelectDefault01(): Bool = region r {
        select {
            case x <- recv({let (_, rx) = Channel.buffered(r, 1); rx}) => x
            case _                                                     => true
        }
    }

    @test
    def testSelectDefault02(): Bool = region r {
        (1 + select {
            case _ <- recv({let (_, rx) = Channel.buffered(r, 2); rx}) => 2
            case _                                                     => 1
        }) == 2
    }

    def recvWithDefault(rx: Receiver[Int32, r]): Int32 \ r = {
        select {
            case x <- Channel.recv(rx) => x
            case _                     => 1
        }
    }

    def mainx(): Int32 \ IO = {
      let (_, rx) = Channel.buffered(Static, 1);
      recvWithDefault(rx)
    }

    @test
    def testSelectDefault03(): Unit \ IO =
      // This test is from a bug report so its form is subtle and intentional
      unchecked_cast(println(mainx()) as _ \ IO)

    @test
    def testSelectRandom01(): Unit \ IO = region r {
        let (tx9, rx9) = Channel.buffered(r, 0);
        let (tx10, rx10) = Channel.buffered(r, 0);
        let (tx11, rx11) = Channel.buffered(r, 0);
        let (tx12, rx12) = Channel.buffered(r, 0);
        let (tx13, rx13) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx13) ; () } @ r; spawn { Channel.send((), tx12) ; () } @ r; spawn { Channel.send((), tx11) ; () } @ r; spawn { Channel.send((), tx10) ; () } @ r; spawn { Channel.send((), tx9) ; () } @ r; select {
        case _ <- recv(rx13) => select {
        case _ <- recv(rx11) => ()
        case _ <- recv(rx11) => ()
        } ; Channel.recv(rx9) ; Channel.recv(rx10) ; Channel.recv(rx12) ; let (tx42, rx42) = Channel.buffered(r, 0);
        let (tx43, rx43) = Channel.buffered(r, 0);
        let (tx44, rx44) = Channel.buffered(r, 0);
        let (tx45, rx45) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx45) ; () } @ r; spawn { Channel.send((), tx44) ; () } @ r; spawn { Channel.send((), tx43) ; () } @ r; spawn { Channel.send((), tx42) ; () } @ r; select {
        case _ <- recv(rx43) => Channel.recv(rx42) ; Channel.recv(rx44) ; Channel.recv(rx45)
        case _ <- recv(rx42) => Channel.recv(rx45) ; Channel.recv(rx43) ; Channel.recv(rx44) ; ()
        }
        case _ <- recv(rx13) => select {
        case _ <- recv(rx11) => ()
        case _ <- recv(rx11) => ()
        } ; Channel.recv(rx9) ; Channel.recv(rx10) ; Channel.recv(rx12) ; let (tx42, rx42) = Channel.buffered(r, 0);
        let (tx43, rx43) = Channel.buffered(r, 0);
        let (tx44, rx44) = Channel.buffered(r, 0);
        let (tx45, rx45) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx45) ; () } @ r; spawn { Channel.send((), tx44) ; () } @ r; spawn { Channel.send((), tx43) ; () } @ r; spawn { Channel.send((), tx42) ; () } @ r; select {
        case _ <- recv(rx43) => Channel.recv(rx42) ; Channel.recv(rx44) ; Channel.recv(rx45) ; ()
        case _ <- recv(rx42) => Channel.recv(rx45) ; Channel.recv(rx43) ; Channel.recv(rx44)
        }
        };
        ()
    }

    @test
    def testSelectRandom02(): Unit \ IO = region r {
        let (tx10, rx10) = Channel.buffered(r, 0);
        let (tx11, rx11) = Channel.buffered(r, 0);
        let (tx12, rx12) = Channel.buffered(r, 0);
        let (tx13, rx13) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx13) ; () } @ r; spawn { Channel.send((), tx12) ; () } @ r; if (false) { spawn { Channel.send((), tx11) ; () } @ r; spawn { Channel.send((), tx10) ; () } @ r; if (true) { () } else { () } } else { spawn { Channel.send((), tx11) ; () } @ r; spawn { Channel.send((), tx10) ; () } @ r; if (true) { () } else { () } } ; select {
        case _ <- recv(rx10) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx11)
        case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx10) ; Channel.recv(rx11) ; ()
        case _ <- recv(rx11) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx10) ; ()
        };
        ()
    }

    @test
    def testSelectRandom03(): Unit \ IO = region r {
        let (tx14, rx14) = Channel.buffered(r, 0);
        let (tx15, rx15) = Channel.buffered(r, 0);
        let (tx16, rx16) = Channel.buffered(r, 0);
        let (tx17, rx17) = Channel.buffered(r, 0);
        let (tx18, rx18) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx18) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx18) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; let (tx141, rx141) = Channel.buffered(r, 0);
        let (tx139, rx139) = Channel.buffered(r, 0);
        let (tx140, rx140) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx141) ; () } @ r; spawn { Channel.send((), tx140) ; () } @ r; spawn { Channel.send((), tx139) ; () } @ r; spawn { Channel.send((), tx141) ; () } @ r; spawn { Channel.send((), tx140) ; () } @ r; spawn { Channel.send((), tx139) ; () } @ r; spawn { Channel.send((), tx141) ; () } @ r; spawn { Channel.send((), tx140) ; () } @ r; spawn { Channel.send((), tx139) ; () } @ r; spawn { select {
        case _ <- recv(rx141) => Channel.recv(rx140) ; Channel.recv(rx139) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx139) => Channel.recv(rx140) ; Channel.recv(rx141) ; ()
        case _ <- recv(rx140) => Channel.recv(rx141) ; Channel.recv(rx139) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx139) => select {
        case _ <- recv(rx140) => ()
        case _ <- recv(rx140) => ()
        } ; Channel.recv(rx141) ; ()
        case _ <- recv(rx141) => Channel.recv(rx140) ; Channel.recv(rx139) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx15) => Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx17) ; Channel.recv(rx14) ; ()
        case _ <- recv(rx14) => Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx15) ; Channel.recv(rx17) ; ()
        case _ <- recv(rx17) => Channel.recv(rx15) ; Channel.recv(rx18) ; select {
        case _ <- recv(rx16) => ()
        case _ <- recv(rx16) => ()
        } ; Channel.recv(rx14)
        case _ <- recv(rx18) => Channel.recv(rx15) ; Channel.recv(rx14) ; Channel.recv(rx17) ; Channel.recv(rx16) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx16) => Channel.recv(rx18) ; Channel.recv(rx17) ; Channel.recv(rx15) ; Channel.recv(rx14) ; ()
        } } @ r;
        ()
    }

    @test
    def testSelectRandom04(): Unit \ IO = region r {
        let (tx10, rx10) = Channel.buffered(r, 0);
        let (tx11, rx11) = Channel.buffered(r, 0);
        let (tx12, rx12) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx12) ; () } @ r; spawn { Channel.send((), tx11) ; () } @ r; spawn { Channel.send((), tx10) ; () } @ r; let (tx40, rx40) = Channel.buffered(r, 0);
        let (tx41, rx41) = Channel.buffered(r, 0);
        let (tx42, rx42) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx42) ; () } @ r; spawn { Channel.send((), tx41) ; () } @ r; spawn { Channel.send((), tx40) ; () } @ r; spawn { Channel.send((), tx42) ; () } @ r; spawn { Channel.send((), tx41) ; () } @ r; spawn { Channel.send((), tx40) ; () } @ r; spawn { Channel.send((), tx42) ; () } @ r; spawn { Channel.send((), tx41) ; () } @ r; spawn { Channel.send((), tx40) ; () } @ r; spawn { Channel.send((), tx42) ; () } @ r; spawn { Channel.send((), tx41) ; () } @ r; spawn { Channel.send((), tx40) ; () } @ r; spawn { select {
        case _ <- recv(rx40) => Channel.recv(rx41) ; Channel.recv(rx42) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx40) => Channel.recv(rx42) ; Channel.recv(rx41) ; ()
        case _ <- recv(rx40) => Channel.recv(rx42) ; Channel.recv(rx41) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx42) => Channel.recv(rx40) ; Channel.recv(rx41) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx42) => Channel.recv(rx40) ; Channel.recv(rx41) ; ()
        case _ <- recv(rx42) => Channel.recv(rx40) ; Channel.recv(rx41) ; ()
        } } @ r; select {
        case _ <- recv(rx10) => Channel.recv(rx12) ; Channel.recv(rx11) ; ()
        };
        ()
    }

    @test
    def testSelectRandom05(): Unit \ IO = region r {
        let (tx14, rx14) = Channel.buffered(r, 0);
        let (tx15, rx15) = Channel.buffered(r, 0);
        let (tx16, rx16) = Channel.buffered(r, 0);
        let (tx17, rx17) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx14) ; Channel.recv(rx15) ; Channel.recv(rx16)
        } } @ r; spawn { select {
        case _ <- recv(rx16) => Channel.recv(rx14) ; Channel.recv(rx15) ; Channel.recv(rx17) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx14) ; let (tx171, rx171) = Channel.buffered(r, 0);
        let (tx172, rx172) = Channel.buffered(r, 0);
        let (tx173, rx173) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx173) ; () } @ r; spawn { Channel.send((), tx172) ; () } @ r; spawn { Channel.send((), tx171) ; () } @ r; spawn { Channel.send((), tx173) ; () } @ r; spawn { Channel.send((), tx172) ; () } @ r; spawn { Channel.send((), tx171) ; () } @ r; spawn { Channel.send((), tx173) ; () } @ r; spawn { Channel.send((), tx172) ; () } @ r; spawn { Channel.send((), tx171) ; () } @ r; spawn { Channel.send((), tx173) ; () } @ r; spawn { Channel.send((), tx172) ; () } @ r; spawn { Channel.send((), tx171) ; () } @ r; spawn { select {
        case _ <- recv(rx172) => Channel.recv(rx173) ; Channel.recv(rx171) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx171) => Channel.recv(rx173) ; Channel.recv(rx172) ; ()
        case _ <- recv(rx173) => Channel.recv(rx172) ; Channel.recv(rx171) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx171) => Channel.recv(rx173) ; Channel.recv(rx172) ; ()
        case _ <- recv(rx173) => Channel.recv(rx171) ; Channel.recv(rx172) ; ()
        case _ <- recv(rx171) => Channel.recv(rx173) ; Channel.recv(rx172) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx171) => Channel.recv(rx173) ; select {
        case _ <- recv(rx172) => ()
        case _ <- recv(rx172) => ()
        } ; ()
        case _ <- recv(rx173) => Channel.recv(rx171) ; Channel.recv(rx172) ; ()
        } } @ r; ()
        case _ <- recv(rx14) => Channel.recv(rx17) ; Channel.recv(rx15) ; Channel.recv(rx16) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx16) ; Channel.recv(rx15) ; Channel.recv(rx14) ; ()
        } } @ r;
        ()
    }

    @test
    def testSelectRandom06(): Unit \ IO = region r {
        let (tx2, rx2) = Channel.buffered(r, 0);
        let (tx3, rx3) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx3) ; () } @ r; spawn { Channel.send((), tx2) ; () } @ r; Channel.recv(rx3) ; select {
        case _ <- recv(rx2) => ()
        case _ <- recv(rx2) => ()
        } ; let (tx24, rx24) = Channel.buffered(r, 0);
        spawn { select {
        case _ <- recv(rx24) => ()
        case _ <- recv(rx24) => ()
        } } @ r; Channel.send((), tx24) ;
        ()
    }

    @test
    def testSelectRandom07(): Unit \ IO = region r {
        let (tx12, rx12) = Channel.buffered(r, 0);
        let (tx13, rx13) = Channel.buffered(r, 0);
        let (tx14, rx14) = Channel.buffered(r, 0);
        let (tx15, rx15) = Channel.buffered(r, 0);
        let (tx16, rx16) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx13) ; () } @ r; spawn { Channel.send((), tx12) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx13) ; () } @ r; spawn { Channel.send((), tx12) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx14) ; () } @ r; spawn { Channel.send((), tx13) ; () } @ r; spawn { Channel.send((), tx12) ; () } @ r; spawn { select {
        case _ <- recv(rx14) => Channel.recv(rx12) ; Channel.recv(rx13) ; Channel.recv(rx15) ; Channel.recv(rx16) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx16) => Channel.recv(rx13) ; Channel.recv(rx14) ; Channel.recv(rx12) ; Channel.recv(rx15) ; ()
        case _ <- recv(rx15) => Channel.recv(rx14) ; Channel.recv(rx13) ; Channel.recv(rx16) ; Channel.recv(rx12) ; ()
        case _ <- recv(rx12) => Channel.recv(rx13) ; Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx14) ; let (tx180, rx180) = Channel.buffered(r, 0);
        let (tx178, rx178) = Channel.buffered(r, 0);
        let (tx179, rx179) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx180) ; () } @ r; spawn { Channel.send((), tx179) ; () } @ r; spawn { Channel.send((), tx178) ; () } @ r; spawn { Channel.send((), tx180) ; () } @ r; spawn { Channel.send((), tx179) ; () } @ r; spawn { Channel.send((), tx178) ; () } @ r; spawn { select {
        case _ <- recv(rx178) => Channel.recv(rx180) ; Channel.recv(rx179)
        case _ <- recv(rx179) => select {
        case _ <- recv(rx178) => ()
        case _ <- recv(rx178) => ()
        } ; Channel.recv(rx180) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx178) => Channel.recv(rx179) ; Channel.recv(rx180) ; ()
        case _ <- recv(rx180) => Channel.recv(rx178) ; Channel.recv(rx179) ; ()
        } } @ r; ()
        } } @ r; spawn { select {
        case _ <- recv(rx14) => Channel.recv(rx15) ; Channel.recv(rx12) ; Channel.recv(rx16) ; Channel.recv(rx13) ; ()
        case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx14) ; Channel.recv(rx15) ; Channel.recv(rx16) ; ()
        } } @ r;
        ()
    }

    @test
    def testSelectRandom08(): Unit \ IO = region r {
        let (tx15, rx15) = Channel.buffered(r, 0);
        let (tx16, rx16) = Channel.buffered(r, 0);
        let (tx17, rx17) = Channel.buffered(r, 0);
        let (tx18, rx18) = Channel.buffered(r, 0);
        let (tx19, rx19) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx19) ; () } @ r; spawn { Channel.send((), tx18) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx19) ; () } @ r; spawn { Channel.send((), tx18) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx19) ; () } @ r; spawn { Channel.send((), tx18) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { Channel.send((), tx19) ; () } @ r; spawn { Channel.send((), tx18) ; () } @ r; spawn { Channel.send((), tx17) ; () } @ r; spawn { Channel.send((), tx16) ; () } @ r; spawn { Channel.send((), tx15) ; () } @ r; spawn { select {
        case _ <- recv(rx15) => Channel.recv(rx17) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx19) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx15) => Channel.recv(rx19) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx17) ; ()
        case _ <- recv(rx18) => Channel.recv(rx19) ; Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx17) ; ()
        case _ <- recv(rx19) => Channel.recv(rx16) ; Channel.recv(rx17) ; Channel.recv(rx18) ; Channel.recv(rx15) ; ()
        case _ <- recv(rx17) => Channel.recv(rx15) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx19) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx18) ; Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx19) ; ()
        case _ <- recv(rx15) => Channel.recv(rx17) ; Channel.recv(rx16) ; Channel.recv(rx18) ; Channel.recv(rx19) ; ()
        case _ <- recv(rx16) => Channel.recv(rx15) ; Channel.recv(rx18) ; Channel.recv(rx17) ; Channel.recv(rx19) ; ()
        case _ <- recv(rx19) => Channel.recv(rx15) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx17) ; ()
        } } @ r; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx19) ; Channel.recv(rx15) ; ()
        case _ <- recv(rx16) => Channel.recv(rx18) ; select {
        case _ <- recv(rx15) => ()
        case _ <- recv(rx15) => ()
        } ; Channel.recv(rx17) ; Channel.recv(rx19) ; let (tx214, rx214) = Channel.buffered(r, 0);
        let (tx215, rx215) = Channel.buffered(r, 0);
        let (tx216, rx216) = Channel.buffered(r, 0);
        let (tx217, rx217) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx217) ; () } @ r; spawn { Channel.send((), tx216) ; () } @ r; spawn { Channel.send((), tx215) ; () } @ r; spawn { Channel.send((), tx214) ; () } @ r; select {
        case _ <- recv(rx215) => Channel.recv(rx214) ; Channel.recv(rx216) ; Channel.recv(rx217) ; ()
        case _ <- recv(rx214) => Channel.recv(rx216) ; Channel.recv(rx215) ; Channel.recv(rx217) ; ()
        case _ <- recv(rx216) => Channel.recv(rx214) ; Channel.recv(rx217) ; Channel.recv(rx215)
        }
        } } @ r;
        ()
    }

    @test
    def testSelectRandom09(): Unit \ IO = region r {
        let (tx6, rx6) = Channel.buffered(r, 0);
        let (tx7, rx7) = Channel.buffered(r, 0);
        let (tx8, rx8) = Channel.buffered(r, 0);
        let (tx9, rx9) = Channel.buffered(r, 0);
        spawn { select {
        case _ <- recv(rx8) => ()
        case _ <- recv(rx8) => ()
        } ; Channel.send((), tx9) ; () } @ r; spawn { Channel.recv(rx7) ; Channel.send((), tx8) ; () } @ r; spawn { select {
        case _ <- recv(rx6) => ()
        case _ <- recv(rx6) => ()
        } ; Channel.send((), tx7) ; () } @ r; Channel.send((), tx6) ; Channel.recv(rx9);
        ()
    }

    @test
    def testSelectRandom10(): Unit \ IO = region r {
        let (tx10, rx10) = Channel.buffered(r, 0);
        let (tx11, rx11) = Channel.buffered(r, 0);
        let (tx12, rx12) = Channel.buffered(r, 0);
        let (tx13, rx13) = Channel.buffered(r, 0);
        spawn { Channel.send((), tx13) ; () } @ r;
        spawn { Channel.send((), tx12) ; () } @ r;
        spawn { Channel.send((), tx11) ; () } @ r;
        spawn { Channel.send((), tx10) ; () } @ r;
        select {
            case _ <- recv(rx10) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx11)
            case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx10) ; Channel.recv(rx11)
            case _ <- recv(rx11) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx10)
        };
        ()
    }

    @test
    def testSelectSideEffecting01(): Bool = region r {
        def mkChan(): Receiver[Int32] = {
            let (tx, rx) = Channel.buffered(r, 1);
            Channel.send(42, tx);
            rx
        };

        select {
            case x <- recv(mkChan()) => x == 42
        }
    }

    @test
    def testSelectSideEffecting02(): Bool = region r {
        let (tx1, rx1) = Channel.buffered(r, 10);
        let (tx2, rx2) = Channel.buffered(r, 10);
        let (tx3, rx3) = Channel.buffered(r, 10);

        select {
            case x <- recv({Channel.send(1, tx3); rx1}) => x + (Channel.recv(rx2)) + (Channel.recv(rx3)) == 6
            case x <- recv({Channel.send(2, tx2); rx2}) => x + (Channel.recv(rx1)) + (Channel.recv(rx3)) == 6
            case x <- recv({Channel.send(3, tx1); rx3}) => x + (Channel.recv(rx1)) + (Channel.recv(rx2)) == 6
        }
    }

    type alias MyReceiver[a: Type, r: Region] = Receiver[a, r]

    @test
    def testSelectAliasedChannel(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);

        def useChan(mr: MyReceiver[Int32, r]) : Bool =
            select {
                case x <- recv(mr) => x == 42
            };

        Channel.send(42, tx);
        useChan(rx)
    }

    @test
    def testSelectOptionalSyntax01(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        spawn Channel.send(1, tx1) @ r;
        select {
            case x <- Channel.recv(rx1) => x == 1
        }
    }
}
