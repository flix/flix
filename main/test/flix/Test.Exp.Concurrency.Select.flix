mod Test.Exp.Concurrency.Select {

    @test
    def testSelectBuffered01(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        select {
            case x <- recv(rx1) => x == 1
        }
    }

    @test
    def testSelectBuffered02(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
        }
    }

    @test
    def testSelectBuffered03(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (tx3, rx3) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        spawn Channel.send(3, tx3) @ rc;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
            case x <- recv(rx3) => x == 3
        }
    }

    @test
    def testSelectBuffered04(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (tx3, rx3) = Channel.buffered(1);
        let (tx4, rx4) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        spawn Channel.send(3, tx3) @ rc;
        spawn Channel.send(4, tx4) @ rc;
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
    def testSelectBuffered05(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (tx3, rx3) = Channel.buffered(1);
        let (tx4, rx4) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        spawn Channel.send(3, tx3) @ rc;
        spawn Channel.send(4, tx4) @ rc;
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
    def testSelectBuffered06(): Bool \ {Chan, NonDet} = region rc {
        let (_, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (_, rx3) = Channel.buffered(1);
        let (_, rx4) = Channel.buffered(1);
        spawn Channel.send(1, tx2) @ rc;
        select {
            case _ <- recv(rx4) => false
            case _ <- recv(rx3) => false
            case x <- recv(rx2) => x == 1
            case _ <- recv(rx1) => false
        }
    }

    @test
    def testSelectBuffered07(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (tx3, rx3) = Channel.buffered(1);
        let (tx4, rx4) = Channel.buffered(1);
        spawn Channel.send(1i8, tx1) @ rc;
        spawn Channel.send(2i16, tx2) @ rc;
        spawn Channel.send(3i32, tx3) @ rc;
        spawn Channel.send(4i64, tx4) @ rc;
        select {
            case x <- recv(rx4) => x == 4i64
            case x <- recv(rx3) => x == 3i32
            case x <- recv(rx2) => x == 2i16
            case x <- recv(rx1) => x == 1i8
        }
    }

    @test
    def testSelectDefault01(): Bool \ {Chan, NonDet} = {
        select {
            case x <- recv({let (_, rx) = Channel.buffered(1); rx}) => x
            case _                                                  => true
        }
    }

    @test
    def testSelectDefault02(): Bool \ {Chan, NonDet} = {
        (1 + select {
            case _ <- recv({let (_, rx) = Channel.buffered(2); rx}) => 2
            case _                                                  => 1
        }) == 2
    }

    def recvWithDefault(rx: Receiver[Int32]): Int32 \ {Chan, NonDet} = {
        select {
            case x <- Channel.recv(rx) => x
            case _                     => 1
        }
    }

    def mainx(): Int32 \ {Chan, NonDet} = {
      let (_, rx) = Channel.buffered(1);
      recvWithDefault(rx)
    }

    @test
    def testSelectDefault03(): Unit \ IO =
      // This test is from a bug report so its form is subtle and intentional
      unchecked_cast(println(mainx()) as _ \ IO)

    @test
    def testSelectRandom01(): Unit \ {Chan, NonDet} = region rc {
        let (tx9, rx9) = Channel.buffered(0);
        let (tx10, rx10) = Channel.buffered(0);
        let (tx11, rx11) = Channel.buffered(0);
        let (tx12, rx12) = Channel.buffered(0);
        let (tx13, rx13) = Channel.buffered(0);
        spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; spawn { Channel.send((), tx9) ; () } @ rc; select {
        case _ <- recv(rx13) => select {
        case _ <- recv(rx11) => ()
        case _ <- recv(rx11) => ()
        } ; Channel.recv(rx9) ; Channel.recv(rx10) ; Channel.recv(rx12) ; let (tx42, rx42) = Channel.buffered(0);
        let (tx43, rx43) = Channel.buffered(0);
        let (tx44, rx44) = Channel.buffered(0);
        let (tx45, rx45) = Channel.buffered(0);
        spawn { Channel.send((), tx45) ; () } @ rc; spawn { Channel.send((), tx44) ; () } @ rc; spawn { Channel.send((), tx43) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; select {
        case _ <- recv(rx43) => Channel.recv(rx42) ; Channel.recv(rx44) ; Channel.recv(rx45)
        case _ <- recv(rx42) => Channel.recv(rx45) ; Channel.recv(rx43) ; Channel.recv(rx44) ; ()
        }
        case _ <- recv(rx13) => select {
        case _ <- recv(rx11) => ()
        case _ <- recv(rx11) => ()
        } ; Channel.recv(rx9) ; Channel.recv(rx10) ; Channel.recv(rx12) ; let (tx42, rx42) = Channel.buffered(0);
        let (tx43, rx43) = Channel.buffered(0);
        let (tx44, rx44) = Channel.buffered(0);
        let (tx45, rx45) = Channel.buffered(0);
        spawn { Channel.send((), tx45) ; () } @ rc; spawn { Channel.send((), tx44) ; () } @ rc; spawn { Channel.send((), tx43) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; select {
        case _ <- recv(rx43) => Channel.recv(rx42) ; Channel.recv(rx44) ; Channel.recv(rx45) ; ()
        case _ <- recv(rx42) => Channel.recv(rx45) ; Channel.recv(rx43) ; Channel.recv(rx44)
        }
        };
        ()
    }

    @test
    def testSelectRandom02(): Unit \ {Chan, NonDet} = region rc {
        let (tx10, rx10) = Channel.buffered(0);
        let (tx11, rx11) = Channel.buffered(0);
        let (tx12, rx12) = Channel.buffered(0);
        let (tx13, rx13) = Channel.buffered(0);
        spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; if (false) { spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; if (true) { () } else { () } } else { spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; if (true) { () } else { () } } ; select {
        case _ <- recv(rx10) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx11)
        case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx10) ; Channel.recv(rx11) ; ()
        case _ <- recv(rx11) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx10) ; ()
        };
        ()
    }

    @test
    def testSelectRandom04(): Unit \ {Chan, NonDet} = region rc {
        let (tx10, rx10) = Channel.buffered(0);
        let (tx11, rx11) = Channel.buffered(0);
        let (tx12, rx12) = Channel.buffered(0);
        spawn { Channel.send((), tx12) ; () } @ rc; spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; let (tx40, rx40) = Channel.buffered(0);
        let (tx41, rx41) = Channel.buffered(0);
        let (tx42, rx42) = Channel.buffered(0);
        spawn { Channel.send((), tx42) ; () } @ rc; spawn { Channel.send((), tx41) ; () } @ rc; spawn { Channel.send((), tx40) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; spawn { Channel.send((), tx41) ; () } @ rc; spawn { Channel.send((), tx40) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; spawn { Channel.send((), tx41) ; () } @ rc; spawn { Channel.send((), tx40) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; spawn { Channel.send((), tx41) ; () } @ rc; spawn { Channel.send((), tx40) ; () } @ rc; spawn { select {
        case _ <- recv(rx40) => Channel.recv(rx41) ; Channel.recv(rx42) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx40) => Channel.recv(rx42) ; Channel.recv(rx41) ; ()
        case _ <- recv(rx40) => Channel.recv(rx42) ; Channel.recv(rx41) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx42) => Channel.recv(rx40) ; Channel.recv(rx41) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx42) => Channel.recv(rx40) ; Channel.recv(rx41) ; ()
        case _ <- recv(rx42) => Channel.recv(rx40) ; Channel.recv(rx41) ; ()
        } } @ rc; select {
        case _ <- recv(rx10) => Channel.recv(rx12) ; Channel.recv(rx11) ; ()
        };
        ()
    }

    @test
    def testSelectRandom06(): Unit \ {Chan, NonDet} = region rc {
        let (tx2, rx2) = Channel.buffered(0);
        let (tx3, rx3) = Channel.buffered(0);
        spawn { Channel.send((), tx3) ; () } @ rc; spawn { Channel.send((), tx2) ; () } @ rc; Channel.recv(rx3) ; select {
        case _ <- recv(rx2) => ()
        case _ <- recv(rx2) => ()
        } ; let (tx24, rx24) = Channel.buffered(0);
        spawn { select {
        case _ <- recv(rx24) => ()
        case _ <- recv(rx24) => ()
        } } @ rc; Channel.send((), tx24) ;
        ()
    }

    @test
    def testSelectRandom09(): Unit \ {Chan, NonDet} = region rc {
        let (tx6, rx6) = Channel.buffered(0);
        let (tx7, rx7) = Channel.buffered(0);
        let (tx8, rx8) = Channel.buffered(0);
        let (tx9, rx9) = Channel.buffered(0);
        spawn { select {
        case _ <- recv(rx8) => ()
        case _ <- recv(rx8) => ()
        } ; Channel.send((), tx9) ; () } @ rc; spawn { Channel.recv(rx7) ; Channel.send((), tx8) ; () } @ rc; spawn { select {
        case _ <- recv(rx6) => ()
        case _ <- recv(rx6) => ()
        } ; Channel.send((), tx7) ; () } @ rc; Channel.send((), tx6) ; Channel.recv(rx9);
        ()
    }

    @test
    def testSelectRandom10(): Unit \ {Chan, NonDet} = region rc {
        let (tx10, rx10) = Channel.buffered(0);
        let (tx11, rx11) = Channel.buffered(0);
        let (tx12, rx12) = Channel.buffered(0);
        let (tx13, rx13) = Channel.buffered(0);
        spawn { Channel.send((), tx13) ; () } @ rc;
        spawn { Channel.send((), tx12) ; () } @ rc;
        spawn { Channel.send((), tx11) ; () } @ rc;
        spawn { Channel.send((), tx10) ; () } @ rc;
        select {
            case _ <- recv(rx10) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx11)
            case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx10) ; Channel.recv(rx11)
            case _ <- recv(rx11) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx10)
        };
        ()
    }

    @test
    def testSelectSideEffecting01(): Bool \ {Chan, NonDet} = {
        def mkChan(): Receiver[Int32] = {
            let (tx, rx) = Channel.buffered(1);
            Channel.send(42, tx);
            rx
        };

        select {
            case x <- recv(mkChan()) => x == 42
        }
    }

    @test
    def testSelectSideEffecting02(): Bool \ {Chan, NonDet} = {
        let (tx1, rx1) = Channel.buffered(10);
        let (tx2, rx2) = Channel.buffered(10);
        let (tx3, rx3) = Channel.buffered(10);

        select {
            case x <- recv({Channel.send(1, tx3); rx1}) => x + (Channel.recv(rx2)) + (Channel.recv(rx3)) == 6
            case x <- recv({Channel.send(2, tx2); rx2}) => x + (Channel.recv(rx1)) + (Channel.recv(rx3)) == 6
            case x <- recv({Channel.send(3, tx1); rx3}) => x + (Channel.recv(rx1)) + (Channel.recv(rx2)) == 6
        }
    }

    type alias MyReceiver[a: Type] = Receiver[a]

    @test
    def testSelectAliasedChannel(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);

        def useChan(mr: MyReceiver[Int32]) : Bool =
            select {
                case x <- recv(mr) => x == 42
            };

        Channel.send(42, tx);
        useChan(rx)
    }

    @test
    def testSelectOptionalSyntax01(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        select {
            case x <- Channel.recv(rx1) => x == 1
        }
    }
}
