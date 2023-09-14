mod Test.Exp.Concurrency.Select {

    @Test
    def testSelectBuffered01(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        spawn Channel.send(1, tx1) @ rc;
        select {
            case x <- recv(rx1) => x == 1
        }
    }

    @Test
    def testSelectBuffered02(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
        }
    }

    @Test
    def testSelectBuffered03(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        let (tx3, rx3) = Channel.buffered(rc, 1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        spawn Channel.send(3, tx3) @ rc;
        select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
            case x <- recv(rx3) => x == 3
        }
    }

    @Test
    def testSelectBuffered04(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        let (tx3, rx3) = Channel.buffered(rc, 1);
        let (tx4, rx4) = Channel.buffered(rc, 1);
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

    @Test
    def testSelectBuffered05(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        let (tx3, rx3) = Channel.buffered(rc, 1);
        let (tx4, rx4) = Channel.buffered(rc, 1);
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

    @Test
    def testSelectBuffered06(): Bool \ IO = region rc {
        let (_, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        let (_, rx3) = Channel.buffered(rc, 1);
        let (_, rx4) = Channel.buffered(rc, 1);
        spawn Channel.send(1, tx2) @ rc;
        select {
            case _ <- recv(rx4) => false
            case _ <- recv(rx3) => false
            case x <- recv(rx2) => x == 1
            case _ <- recv(rx1) => false
        }
    }

    @Test
    def testSelectBuffered07(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        let (tx3, rx3) = Channel.buffered(rc, 1);
        let (tx4, rx4) = Channel.buffered(rc, 1);
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

    @Test
    def testSelectDefault01(): Bool = region rc {
        select {
            case x <- recv({let (_, rx) = Channel.buffered(rc, 1); rx}) => x
            case _                                                     => true
        }
    }

    @Test
    def testSelectDefault02(): Bool = region rc {
        (1 + select {
            case _ <- recv({let (_, rx) = Channel.buffered(rc, 2); rx}) => 2
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

    @Test
    def testSelectDefault03(): Unit \ IO =
      // This test is from a bug report so its form is subtle and intentional
      unchecked_cast(println(mainx()) as _ \ IO)

    @Test
    def testSelectRandom01(): Unit \ IO = region rc {
        let (tx9, rx9) = Channel.buffered(rc, 0);
        let (tx10, rx10) = Channel.buffered(rc, 0);
        let (tx11, rx11) = Channel.buffered(rc, 0);
        let (tx12, rx12) = Channel.buffered(rc, 0);
        let (tx13, rx13) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; spawn { Channel.send((), tx9) ; () } @ rc; select {
        case _ <- recv(rx13) => select {
        case _ <- recv(rx11) => ()
        case _ <- recv(rx11) => ()
        } ; Channel.recv(rx9) ; Channel.recv(rx10) ; Channel.recv(rx12) ; let (tx42, rx42) = Channel.buffered(rc, 0);
        let (tx43, rx43) = Channel.buffered(rc, 0);
        let (tx44, rx44) = Channel.buffered(rc, 0);
        let (tx45, rx45) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx45) ; () } @ rc; spawn { Channel.send((), tx44) ; () } @ rc; spawn { Channel.send((), tx43) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; select {
        case _ <- recv(rx43) => Channel.recv(rx42) ; Channel.recv(rx44) ; Channel.recv(rx45)
        case _ <- recv(rx42) => Channel.recv(rx45) ; Channel.recv(rx43) ; Channel.recv(rx44) ; ()
        }
        case _ <- recv(rx13) => select {
        case _ <- recv(rx11) => ()
        case _ <- recv(rx11) => ()
        } ; Channel.recv(rx9) ; Channel.recv(rx10) ; Channel.recv(rx12) ; let (tx42, rx42) = Channel.buffered(rc, 0);
        let (tx43, rx43) = Channel.buffered(rc, 0);
        let (tx44, rx44) = Channel.buffered(rc, 0);
        let (tx45, rx45) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx45) ; () } @ rc; spawn { Channel.send((), tx44) ; () } @ rc; spawn { Channel.send((), tx43) ; () } @ rc; spawn { Channel.send((), tx42) ; () } @ rc; select {
        case _ <- recv(rx43) => Channel.recv(rx42) ; Channel.recv(rx44) ; Channel.recv(rx45) ; ()
        case _ <- recv(rx42) => Channel.recv(rx45) ; Channel.recv(rx43) ; Channel.recv(rx44)
        }
        };
        ()
    }

    @Test
    def testSelectRandom02(): Unit \ IO = region rc {
        let (tx10, rx10) = Channel.buffered(rc, 0);
        let (tx11, rx11) = Channel.buffered(rc, 0);
        let (tx12, rx12) = Channel.buffered(rc, 0);
        let (tx13, rx13) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; if (false) { spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; if (true) { () } else { () } } else { spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; if (true) { () } else { () } } ; select {
        case _ <- recv(rx10) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx11)
        case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx10) ; Channel.recv(rx11) ; ()
        case _ <- recv(rx11) => Channel.recv(rx13) ; Channel.recv(rx12) ; Channel.recv(rx10) ; ()
        };
        ()
    }

    @Test
    def testSelectRandom03(): Unit \ IO = region rc {
        let (tx14, rx14) = Channel.buffered(rc, 0);
        let (tx15, rx15) = Channel.buffered(rc, 0);
        let (tx16, rx16) = Channel.buffered(rc, 0);
        let (tx17, rx17) = Channel.buffered(rc, 0);
        let (tx18, rx18) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx18) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx18) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; let (tx141, rx141) = Channel.buffered(rc, 0);
        let (tx139, rx139) = Channel.buffered(rc, 0);
        let (tx140, rx140) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx141) ; () } @ rc; spawn { Channel.send((), tx140) ; () } @ rc; spawn { Channel.send((), tx139) ; () } @ rc; spawn { Channel.send((), tx141) ; () } @ rc; spawn { Channel.send((), tx140) ; () } @ rc; spawn { Channel.send((), tx139) ; () } @ rc; spawn { Channel.send((), tx141) ; () } @ rc; spawn { Channel.send((), tx140) ; () } @ rc; spawn { Channel.send((), tx139) ; () } @ rc; spawn { select {
        case _ <- recv(rx141) => Channel.recv(rx140) ; Channel.recv(rx139) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx139) => Channel.recv(rx140) ; Channel.recv(rx141) ; ()
        case _ <- recv(rx140) => Channel.recv(rx141) ; Channel.recv(rx139) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx139) => select {
        case _ <- recv(rx140) => ()
        case _ <- recv(rx140) => ()
        } ; Channel.recv(rx141) ; ()
        case _ <- recv(rx141) => Channel.recv(rx140) ; Channel.recv(rx139) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx15) => Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx17) ; Channel.recv(rx14) ; ()
        case _ <- recv(rx14) => Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx15) ; Channel.recv(rx17) ; ()
        case _ <- recv(rx17) => Channel.recv(rx15) ; Channel.recv(rx18) ; select {
        case _ <- recv(rx16) => ()
        case _ <- recv(rx16) => ()
        } ; Channel.recv(rx14)
        case _ <- recv(rx18) => Channel.recv(rx15) ; Channel.recv(rx14) ; Channel.recv(rx17) ; Channel.recv(rx16) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx16) => Channel.recv(rx18) ; Channel.recv(rx17) ; Channel.recv(rx15) ; Channel.recv(rx14) ; ()
        } } @ rc;
        ()
    }

    @Test
    def testSelectRandom04(): Unit \ IO = region rc {
        let (tx10, rx10) = Channel.buffered(rc, 0);
        let (tx11, rx11) = Channel.buffered(rc, 0);
        let (tx12, rx12) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx12) ; () } @ rc; spawn { Channel.send((), tx11) ; () } @ rc; spawn { Channel.send((), tx10) ; () } @ rc; let (tx40, rx40) = Channel.buffered(rc, 0);
        let (tx41, rx41) = Channel.buffered(rc, 0);
        let (tx42, rx42) = Channel.buffered(rc, 0);
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

    @Test
    def testSelectRandom05(): Unit \ IO = region rc {
        let (tx14, rx14) = Channel.buffered(rc, 0);
        let (tx15, rx15) = Channel.buffered(rc, 0);
        let (tx16, rx16) = Channel.buffered(rc, 0);
        let (tx17, rx17) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx14) ; Channel.recv(rx15) ; Channel.recv(rx16)
        } } @ rc; spawn { select {
        case _ <- recv(rx16) => Channel.recv(rx14) ; Channel.recv(rx15) ; Channel.recv(rx17) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx14) ; let (tx171, rx171) = Channel.buffered(rc, 0);
        let (tx172, rx172) = Channel.buffered(rc, 0);
        let (tx173, rx173) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx173) ; () } @ rc; spawn { Channel.send((), tx172) ; () } @ rc; spawn { Channel.send((), tx171) ; () } @ rc; spawn { Channel.send((), tx173) ; () } @ rc; spawn { Channel.send((), tx172) ; () } @ rc; spawn { Channel.send((), tx171) ; () } @ rc; spawn { Channel.send((), tx173) ; () } @ rc; spawn { Channel.send((), tx172) ; () } @ rc; spawn { Channel.send((), tx171) ; () } @ rc; spawn { Channel.send((), tx173) ; () } @ rc; spawn { Channel.send((), tx172) ; () } @ rc; spawn { Channel.send((), tx171) ; () } @ rc; spawn { select {
        case _ <- recv(rx172) => Channel.recv(rx173) ; Channel.recv(rx171) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx171) => Channel.recv(rx173) ; Channel.recv(rx172) ; ()
        case _ <- recv(rx173) => Channel.recv(rx172) ; Channel.recv(rx171) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx171) => Channel.recv(rx173) ; Channel.recv(rx172) ; ()
        case _ <- recv(rx173) => Channel.recv(rx171) ; Channel.recv(rx172) ; ()
        case _ <- recv(rx171) => Channel.recv(rx173) ; Channel.recv(rx172) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx171) => Channel.recv(rx173) ; select {
        case _ <- recv(rx172) => ()
        case _ <- recv(rx172) => ()
        } ; ()
        case _ <- recv(rx173) => Channel.recv(rx171) ; Channel.recv(rx172) ; ()
        } } @ rc; ()
        case _ <- recv(rx14) => Channel.recv(rx17) ; Channel.recv(rx15) ; Channel.recv(rx16) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx16) ; Channel.recv(rx15) ; Channel.recv(rx14) ; ()
        } } @ rc;
        ()
    }

    @Test
    def testSelectRandom06(): Unit \ IO = region rc {
        let (tx2, rx2) = Channel.buffered(rc, 0);
        let (tx3, rx3) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx3) ; () } @ rc; spawn { Channel.send((), tx2) ; () } @ rc; Channel.recv(rx3) ; select {
        case _ <- recv(rx2) => ()
        case _ <- recv(rx2) => ()
        } ; let (tx24, rx24) = Channel.buffered(rc, 0);
        spawn { select {
        case _ <- recv(rx24) => ()
        case _ <- recv(rx24) => ()
        } } @ rc; Channel.send((), tx24) ;
        ()
    }

    @Test
    def testSelectRandom07(): Unit \ IO = region rc {
        let (tx12, rx12) = Channel.buffered(rc, 0);
        let (tx13, rx13) = Channel.buffered(rc, 0);
        let (tx14, rx14) = Channel.buffered(rc, 0);
        let (tx15, rx15) = Channel.buffered(rc, 0);
        let (tx16, rx16) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx14) ; () } @ rc; spawn { Channel.send((), tx13) ; () } @ rc; spawn { Channel.send((), tx12) ; () } @ rc; spawn { select {
        case _ <- recv(rx14) => Channel.recv(rx12) ; Channel.recv(rx13) ; Channel.recv(rx15) ; Channel.recv(rx16) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx16) => Channel.recv(rx13) ; Channel.recv(rx14) ; Channel.recv(rx12) ; Channel.recv(rx15) ; ()
        case _ <- recv(rx15) => Channel.recv(rx14) ; Channel.recv(rx13) ; Channel.recv(rx16) ; Channel.recv(rx12) ; ()
        case _ <- recv(rx12) => Channel.recv(rx13) ; Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx14) ; let (tx180, rx180) = Channel.buffered(rc, 0);
        let (tx178, rx178) = Channel.buffered(rc, 0);
        let (tx179, rx179) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx180) ; () } @ rc; spawn { Channel.send((), tx179) ; () } @ rc; spawn { Channel.send((), tx178) ; () } @ rc; spawn { Channel.send((), tx180) ; () } @ rc; spawn { Channel.send((), tx179) ; () } @ rc; spawn { Channel.send((), tx178) ; () } @ rc; spawn { select {
        case _ <- recv(rx178) => Channel.recv(rx180) ; Channel.recv(rx179)
        case _ <- recv(rx179) => select {
        case _ <- recv(rx178) => ()
        case _ <- recv(rx178) => ()
        } ; Channel.recv(rx180) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx178) => Channel.recv(rx179) ; Channel.recv(rx180) ; ()
        case _ <- recv(rx180) => Channel.recv(rx178) ; Channel.recv(rx179) ; ()
        } } @ rc; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx14) => Channel.recv(rx15) ; Channel.recv(rx12) ; Channel.recv(rx16) ; Channel.recv(rx13) ; ()
        case _ <- recv(rx13) => Channel.recv(rx12) ; Channel.recv(rx14) ; Channel.recv(rx15) ; Channel.recv(rx16) ; ()
        } } @ rc;
        ()
    }

    @Test
    def testSelectRandom08(): Unit \ IO = region rc {
        let (tx15, rx15) = Channel.buffered(rc, 0);
        let (tx16, rx16) = Channel.buffered(rc, 0);
        let (tx17, rx17) = Channel.buffered(rc, 0);
        let (tx18, rx18) = Channel.buffered(rc, 0);
        let (tx19, rx19) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx19) ; () } @ rc; spawn { Channel.send((), tx18) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx19) ; () } @ rc; spawn { Channel.send((), tx18) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx19) ; () } @ rc; spawn { Channel.send((), tx18) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { Channel.send((), tx19) ; () } @ rc; spawn { Channel.send((), tx18) ; () } @ rc; spawn { Channel.send((), tx17) ; () } @ rc; spawn { Channel.send((), tx16) ; () } @ rc; spawn { Channel.send((), tx15) ; () } @ rc; spawn { select {
        case _ <- recv(rx15) => Channel.recv(rx17) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx19) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx15) => Channel.recv(rx19) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx17) ; ()
        case _ <- recv(rx18) => Channel.recv(rx19) ; Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx17) ; ()
        case _ <- recv(rx19) => Channel.recv(rx16) ; Channel.recv(rx17) ; Channel.recv(rx18) ; Channel.recv(rx15) ; ()
        case _ <- recv(rx17) => Channel.recv(rx15) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx19) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx18) ; Channel.recv(rx15) ; Channel.recv(rx16) ; Channel.recv(rx19) ; ()
        case _ <- recv(rx15) => Channel.recv(rx17) ; Channel.recv(rx16) ; Channel.recv(rx18) ; Channel.recv(rx19) ; ()
        case _ <- recv(rx16) => Channel.recv(rx15) ; Channel.recv(rx18) ; Channel.recv(rx17) ; Channel.recv(rx19) ; ()
        case _ <- recv(rx19) => Channel.recv(rx15) ; Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx17) ; ()
        } } @ rc; spawn { select {
        case _ <- recv(rx17) => Channel.recv(rx18) ; Channel.recv(rx16) ; Channel.recv(rx19) ; Channel.recv(rx15) ; ()
        case _ <- recv(rx16) => Channel.recv(rx18) ; select {
        case _ <- recv(rx15) => ()
        case _ <- recv(rx15) => ()
        } ; Channel.recv(rx17) ; Channel.recv(rx19) ; let (tx214, rx214) = Channel.buffered(rc, 0);
        let (tx215, rx215) = Channel.buffered(rc, 0);
        let (tx216, rx216) = Channel.buffered(rc, 0);
        let (tx217, rx217) = Channel.buffered(rc, 0);
        spawn { Channel.send((), tx217) ; () } @ rc; spawn { Channel.send((), tx216) ; () } @ rc; spawn { Channel.send((), tx215) ; () } @ rc; spawn { Channel.send((), tx214) ; () } @ rc; select {
        case _ <- recv(rx215) => Channel.recv(rx214) ; Channel.recv(rx216) ; Channel.recv(rx217) ; ()
        case _ <- recv(rx214) => Channel.recv(rx216) ; Channel.recv(rx215) ; Channel.recv(rx217) ; ()
        case _ <- recv(rx216) => Channel.recv(rx214) ; Channel.recv(rx217) ; Channel.recv(rx215)
        }
        } } @ rc;
        ()
    }

    @Test
    def testSelectRandom09(): Unit \ IO = region rc {
        let (tx6, rx6) = Channel.buffered(rc, 0);
        let (tx7, rx7) = Channel.buffered(rc, 0);
        let (tx8, rx8) = Channel.buffered(rc, 0);
        let (tx9, rx9) = Channel.buffered(rc, 0);
        spawn { select {
        case _ <- recv(rx8) => ()
        case _ <- recv(rx8) => ()
        } ; Channel.send((), tx9) ; () } @ rc; spawn { Channel.recv(rx7) ; Channel.send((), tx8) ; () } @ rc; spawn { select {
        case _ <- recv(rx6) => ()
        case _ <- recv(rx6) => ()
        } ; Channel.send((), tx7) ; () } @ rc; Channel.send((), tx6) ; Channel.recv(rx9);
        ()
    }

    @Test
    def testSelectRandom10(): Unit \ IO = region rc {
        let (tx10, rx10) = Channel.buffered(rc, 0);
        let (tx11, rx11) = Channel.buffered(rc, 0);
        let (tx12, rx12) = Channel.buffered(rc, 0);
        let (tx13, rx13) = Channel.buffered(rc, 0);
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

    @Test
    def testSelectSideEffecting01(): Bool = region rc {
        def mkChan(): Receiver[Int32, rc] = {
            let (tx, rx) = Channel.buffered(rc, 1);
            Channel.send(42, tx);
            rx
        };

        select {
            case x <- recv(mkChan()) => x == 42
        }
    }

    @Test
    def testSelectSideEffecting02(): Bool = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 10);
        let (tx2, rx2) = Channel.buffered(rc, 10);
        let (tx3, rx3) = Channel.buffered(rc, 10);

        select {
            case x <- recv({Channel.send(1, tx3); rx1}) => x + (Channel.recv(rx2)) + (Channel.recv(rx3)) == 6
            case x <- recv({Channel.send(2, tx2); rx2}) => x + (Channel.recv(rx1)) + (Channel.recv(rx3)) == 6
            case x <- recv({Channel.send(3, tx1); rx3}) => x + (Channel.recv(rx1)) + (Channel.recv(rx2)) == 6
        }
    }

    type alias MyReceiver[a: Type, r: Region] = Receiver[a, r]

    @Test
    def testSelectAliasedChannel(): Bool = region rc {
        let (tx, rx) = Channel.buffered(rc, 1);

        def useChan(mr: MyReceiver[Int32, rc]) : Bool =
            select {
                case x <- recv(mr) => x == 42
            };

        Channel.send(42, tx);
        useChan(rx)
    }

    @Test
    def testSelectOptionalSyntax01(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        spawn Channel.send(1, tx1) @ rc;
        select {
            case x <- Channel.recv(rx1) => x == 1
        }
    }
}
