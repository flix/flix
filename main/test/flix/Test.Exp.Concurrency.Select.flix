namespace Test/Exp/Concurrency/Select {

    @test
    def testSelectBuffered01(): Bool \ IO =
        let (s1, r1) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s1);
        select {
            case x <- recv(r1) => x == 1
        }

    @test
    def testSelectBuffered02(): Bool \ IO =
        let (s1, r1) = Channel.buffered(Static, 1);
        let (s2, r2) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s1);
        spawn Channel.send(2, s2);
        select {
            case x <- recv(r1) => x == 1
            case x <- recv(r2) => x == 2
        }

    @test
    def testSelectBuffered03(): Bool \ IO =
        let (s1, r1) = Channel.buffered(Static, 1);
        let (s2, r2) = Channel.buffered(Static, 1);
        let (s3, r3) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s1);
        spawn Channel.send(2, s2);
        spawn Channel.send(3, s3);
        select {
            case x <- recv(r1) => x == 1
            case x <- recv(r2) => x == 2
            case x <- recv(r3) => x == 3
        }

    @test
    def testSelectBuffered04(): Bool \ IO =
        let (s1, r1) = Channel.buffered(Static, 1);
        let (s2, r2) = Channel.buffered(Static, 1);
        let (s3, r3) = Channel.buffered(Static, 1);
        let (s4, r4) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s1);
        spawn Channel.send(2, s2);
        spawn Channel.send(3, s3);
        spawn Channel.send(4, s4);
        select {
            case x <- recv(r1) => x == 1
            case x <- recv(r1) => x == 1
            case x <- recv(r2) => x == 2
            case x <- recv(r2) => x == 2
            case x <- recv(r3) => x == 3
            case x <- recv(r3) => x == 3
            case x <- recv(r4) => x == 4
            case x <- recv(r4) => x == 4
        }

    @test
    def testSelectBuffered05(): Bool \ IO =
        let (s1, r1) = Channel.buffered(Static, 1);
        let (s2, r2) = Channel.buffered(Static, 1);
        let (s3, r3) = Channel.buffered(Static, 1);
        let (s4, r4) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s1);
        spawn Channel.send(2, s2);
        spawn Channel.send(3, s3);
        spawn Channel.send(4, s4);
        select {
            case x <- recv(r4) => x == 4
            case x <- recv(r3) => x == 3
            case x <- recv(r2) => x == 2
            case x <- recv(r1) => x == 1
            case x <- recv(r4) => x == 4
            case x <- recv(r3) => x == 3
            case x <- recv(r2) => x == 2
            case x <- recv(r1) => x == 1
        }

    @test
    def testSelectBuffered06(): Bool \ IO =
        let (_, r1) = Channel.buffered(Static, 1);
        let (s2, r2) = Channel.buffered(Static, 1);
        let (_, r3) = Channel.buffered(Static, 1);
        let (_, r4) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s2);
        select {
            case _ <- recv(r4) => false
            case _ <- recv(r3) => false
            case x <- recv(r2) => x == 1
            case _ <- recv(r1) => false
        }

    @test
    def testSelectBuffered07(): Bool \ IO =
        let (s1, r1) = Channel.buffered(Static, 1);
        let (s2, r2) = Channel.buffered(Static, 1);
        let (s3, r3) = Channel.buffered(Static, 1);
        let (s4, r4) = Channel.buffered(Static, 1);
        spawn Channel.send(1i8, s1);
        spawn Channel.send(2i16, s2);
        spawn Channel.send(3i32, s3);
        spawn Channel.send(4i64, s4);
        select {
            case x <- recv(r4) => x == 4i64
            case x <- recv(r3) => x == 3i32
            case x <- recv(r2) => x == 2i16
            case x <- recv(r1) => x == 1i8
        }

    @test
    def testSelectDefault01(): Bool \ IO =
        select {
            case x <- recv({let (_, r) = Channel.buffered(Static, 1); r}) => x
            case _                                                => true
        }

    @test
    def testSelectDefault02(): Bool \ IO =
        (1 + select {
            case _ <- recv({let (_, r) = Channel.buffered(Static, 2); r}) => 2
            case _                                                => 1
        }) == 2

    @test
    def testSelectRandom01(): Unit \ IO = {
        let (s9, r9) = Channel.buffered(Static, 0);
        let (s10, r10) = Channel.buffered(Static, 0);
        let (s11, r11) = Channel.buffered(Static, 0);
        let (s12, r12) = Channel.buffered(Static, 0);
        let (s13, r13) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s13) ; () } ; spawn { Channel.send((), s12) ; () } ; spawn { Channel.send((), s11) ; () } ; spawn { Channel.send((), s10) ; () } ; spawn { Channel.send((), s9) ; () } ; select {
        case _ <- recv(r13) => select {
        case _ <- recv(r11) => ()
        case _ <- recv(r11) => ()
        } ; Channel.recv(r9) ; Channel.recv(r10) ; Channel.recv(r12) ; let (s42, r42) = Channel.buffered(Static, 0);
        let (s43, r43) = Channel.buffered(Static, 0);
        let (s44, r44) = Channel.buffered(Static, 0);
        let (s45, r45) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s45) ; () } ; spawn { Channel.send((), s44) ; () } ; spawn { Channel.send((), s43) ; () } ; spawn { Channel.send((), s42) ; () } ; select {
        case _ <- recv(r43) => Channel.recv(r42) ; Channel.recv(r44) ; Channel.recv(r45)
        case _ <- recv(r42) => Channel.recv(r45) ; Channel.recv(r43) ; Channel.recv(r44) ; ()
        }
        case _ <- recv(r13) => select {
        case _ <- recv(r11) => ()
        case _ <- recv(r11) => ()
        } ; Channel.recv(r9) ; Channel.recv(r10) ; Channel.recv(r12) ; let (s42, r42) = Channel.buffered(Static, 0);
        let (s43, r43) = Channel.buffered(Static, 0);
        let (s44, r44) = Channel.buffered(Static, 0);
        let (s45, r45) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s45) ; () } ; spawn { Channel.send((), s44) ; () } ; spawn { Channel.send((), s43) ; () } ; spawn { Channel.send((), s42) ; () } ; select {
        case _ <- recv(r43) => Channel.recv(r42) ; Channel.recv(r44) ; Channel.recv(r45) ; ()
        case _ <- recv(r42) => Channel.recv(r45) ; Channel.recv(r43) ; Channel.recv(r44)
        }
        };
        ()
    }

    @test
    def testSelectRandom02(): Unit \ IO = {
        let (s10, r10) = Channel.buffered(Static, 0);
        let (s11, r11) = Channel.buffered(Static, 0);
        let (s12, r12) = Channel.buffered(Static, 0);
        let (s13, r13) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s13) ; () } ; spawn { Channel.send((), s12) ; () } ; if (false) { spawn { Channel.send((), s11) ; () } ; spawn { Channel.send((), s10) ; () } ; if (true) { () } else { () } } else { spawn { Channel.send((), s11) ; () } ; spawn { Channel.send((), s10) ; () } ; if (true) { () } else { () } } ; select {
        case _ <- recv(r10) => Channel.recv(r13) ; Channel.recv(r12) ; Channel.recv(r11)
        case _ <- recv(r13) => Channel.recv(r12) ; Channel.recv(r10) ; Channel.recv(r11) ; ()
        case _ <- recv(r11) => Channel.recv(r13) ; Channel.recv(r12) ; Channel.recv(r10) ; ()
        };
        ()
    }

    @test
    def testSelectRandom03(): Unit \ IO = {
        let (s14, r14) = Channel.buffered(Static, 0);
        let (s15, r15) = Channel.buffered(Static, 0);
        let (s16, r16) = Channel.buffered(Static, 0);
        let (s17, r17) = Channel.buffered(Static, 0);
        let (s18, r18) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s18) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s18) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; let (s141, r141) = Channel.buffered(Static, 0);
        let (s139, r139) = Channel.buffered(Static, 0);
        let (s140, r140) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s141) ; () } ; spawn { Channel.send((), s140) ; () } ; spawn { Channel.send((), s139) ; () } ; spawn { Channel.send((), s141) ; () } ; spawn { Channel.send((), s140) ; () } ; spawn { Channel.send((), s139) ; () } ; spawn { Channel.send((), s141) ; () } ; spawn { Channel.send((), s140) ; () } ; spawn { Channel.send((), s139) ; () } ; spawn { select {
        case _ <- recv(r141) => Channel.recv(r140) ; Channel.recv(r139) ; ()
        } } ; spawn { select {
        case _ <- recv(r139) => Channel.recv(r140) ; Channel.recv(r141) ; ()
        case _ <- recv(r140) => Channel.recv(r141) ; Channel.recv(r139) ; ()
        } } ; spawn { select {
        case _ <- recv(r139) => select {
        case _ <- recv(r140) => ()
        case _ <- recv(r140) => ()
        } ; Channel.recv(r141) ; ()
        case _ <- recv(r141) => Channel.recv(r140) ; Channel.recv(r139) ; ()
        } } ; spawn { select {
        case _ <- recv(r15) => Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r17) ; Channel.recv(r14) ; ()
        case _ <- recv(r14) => Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r15) ; Channel.recv(r17) ; ()
        case _ <- recv(r17) => Channel.recv(r15) ; Channel.recv(r18) ; select {
        case _ <- recv(r16) => ()
        case _ <- recv(r16) => ()
        } ; Channel.recv(r14)
        case _ <- recv(r18) => Channel.recv(r15) ; Channel.recv(r14) ; Channel.recv(r17) ; Channel.recv(r16) ; ()
        } } ; spawn { select {
        case _ <- recv(r16) => Channel.recv(r18) ; Channel.recv(r17) ; Channel.recv(r15) ; Channel.recv(r14) ; ()
        } } ;
        ()
    }

    @test
    def testSelectRandom04(): Unit \ IO = {
        let (s10, r10) = Channel.buffered(Static, 0);
        let (s11, r11) = Channel.buffered(Static, 0);
        let (s12, r12) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s12) ; () } ; spawn { Channel.send((), s11) ; () } ; spawn { Channel.send((), s10) ; () } ; let (s40, r40) = Channel.buffered(Static, 0);
        let (s41, r41) = Channel.buffered(Static, 0);
        let (s42, r42) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s42) ; () } ; spawn { Channel.send((), s41) ; () } ; spawn { Channel.send((), s40) ; () } ; spawn { Channel.send((), s42) ; () } ; spawn { Channel.send((), s41) ; () } ; spawn { Channel.send((), s40) ; () } ; spawn { Channel.send((), s42) ; () } ; spawn { Channel.send((), s41) ; () } ; spawn { Channel.send((), s40) ; () } ; spawn { Channel.send((), s42) ; () } ; spawn { Channel.send((), s41) ; () } ; spawn { Channel.send((), s40) ; () } ; spawn { select {
        case _ <- recv(r40) => Channel.recv(r41) ; Channel.recv(r42) ; ()
        } } ; spawn { select {
        case _ <- recv(r40) => Channel.recv(r42) ; Channel.recv(r41) ; ()
        case _ <- recv(r40) => Channel.recv(r42) ; Channel.recv(r41) ; ()
        } } ; spawn { select {
        case _ <- recv(r42) => Channel.recv(r40) ; Channel.recv(r41) ; ()
        } } ; spawn { select {
        case _ <- recv(r42) => Channel.recv(r40) ; Channel.recv(r41) ; ()
        case _ <- recv(r42) => Channel.recv(r40) ; Channel.recv(r41) ; ()
        } } ; select {
        case _ <- recv(r10) => Channel.recv(r12) ; Channel.recv(r11) ; ()
        };
        ()
    }

    @test
    def testSelectRandom05(): Unit \ IO = {
        let (s14, r14) = Channel.buffered(Static, 0);
        let (s15, r15) = Channel.buffered(Static, 0);
        let (s16, r16) = Channel.buffered(Static, 0);
        let (s17, r17) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { select {
        case _ <- recv(r17) => Channel.recv(r14) ; Channel.recv(r15) ; Channel.recv(r16)
        } } ; spawn { select {
        case _ <- recv(r16) => Channel.recv(r14) ; Channel.recv(r15) ; Channel.recv(r17) ; ()
        } } ; spawn { select {
        case _ <- recv(r17) => Channel.recv(r15) ; Channel.recv(r16) ; Channel.recv(r14) ; let (s171, r171) = Channel.buffered(Static, 0);
        let (s172, r172) = Channel.buffered(Static, 0);
        let (s173, r173) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s173) ; () } ; spawn { Channel.send((), s172) ; () } ; spawn { Channel.send((), s171) ; () } ; spawn { Channel.send((), s173) ; () } ; spawn { Channel.send((), s172) ; () } ; spawn { Channel.send((), s171) ; () } ; spawn { Channel.send((), s173) ; () } ; spawn { Channel.send((), s172) ; () } ; spawn { Channel.send((), s171) ; () } ; spawn { Channel.send((), s173) ; () } ; spawn { Channel.send((), s172) ; () } ; spawn { Channel.send((), s171) ; () } ; spawn { select {
        case _ <- recv(r172) => Channel.recv(r173) ; Channel.recv(r171) ; ()
        } } ; spawn { select {
        case _ <- recv(r171) => Channel.recv(r173) ; Channel.recv(r172) ; ()
        case _ <- recv(r173) => Channel.recv(r172) ; Channel.recv(r171) ; ()
        } } ; spawn { select {
        case _ <- recv(r171) => Channel.recv(r173) ; Channel.recv(r172) ; ()
        case _ <- recv(r173) => Channel.recv(r171) ; Channel.recv(r172) ; ()
        case _ <- recv(r171) => Channel.recv(r173) ; Channel.recv(r172) ; ()
        } } ; spawn { select {
        case _ <- recv(r171) => Channel.recv(r173) ; select {
        case _ <- recv(r172) => ()
        case _ <- recv(r172) => ()
        } ; ()
        case _ <- recv(r173) => Channel.recv(r171) ; Channel.recv(r172) ; ()
        } } ; ()
        case _ <- recv(r14) => Channel.recv(r17) ; Channel.recv(r15) ; Channel.recv(r16) ; ()
        } } ; spawn { select {
        case _ <- recv(r17) => Channel.recv(r16) ; Channel.recv(r15) ; Channel.recv(r14) ; ()
        } } ;
        ()
    }

    @test
    def testSelectRandom06(): Unit \ IO = {
        let (s2, r2) = Channel.buffered(Static, 0);
        let (s3, r3) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s3) ; () } ; spawn { Channel.send((), s2) ; () } ; Channel.recv(r3) ; select {
        case _ <- recv(r2) => ()
        case _ <- recv(r2) => ()
        } ; let (s24, r24) = Channel.buffered(Static, 0);
        spawn { select {
        case _ <- recv(r24) => ()
        case _ <- recv(r24) => ()
        } } ; Channel.send((), s24) ;
        ()
    }

    @test
    def testSelectRandom07(): Unit \ IO = {
        let (s12, r12) = Channel.buffered(Static, 0);
        let (s13, r13) = Channel.buffered(Static, 0);
        let (s14, r14) = Channel.buffered(Static, 0);
        let (s15, r15) = Channel.buffered(Static, 0);
        let (s16, r16) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s13) ; () } ; spawn { Channel.send((), s12) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s13) ; () } ; spawn { Channel.send((), s12) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s14) ; () } ; spawn { Channel.send((), s13) ; () } ; spawn { Channel.send((), s12) ; () } ; spawn { select {
        case _ <- recv(r14) => Channel.recv(r12) ; Channel.recv(r13) ; Channel.recv(r15) ; Channel.recv(r16) ; ()
        } } ; spawn { select {
        case _ <- recv(r16) => Channel.recv(r13) ; Channel.recv(r14) ; Channel.recv(r12) ; Channel.recv(r15) ; ()
        case _ <- recv(r15) => Channel.recv(r14) ; Channel.recv(r13) ; Channel.recv(r16) ; Channel.recv(r12) ; ()
        case _ <- recv(r12) => Channel.recv(r13) ; Channel.recv(r15) ; Channel.recv(r16) ; Channel.recv(r14) ; let (s180, r180) = Channel.buffered(Static, 0);
        let (s178, r178) = Channel.buffered(Static, 0);
        let (s179, r179) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s180) ; () } ; spawn { Channel.send((), s179) ; () } ; spawn { Channel.send((), s178) ; () } ; spawn { Channel.send((), s180) ; () } ; spawn { Channel.send((), s179) ; () } ; spawn { Channel.send((), s178) ; () } ; spawn { select {
        case _ <- recv(r178) => Channel.recv(r180) ; Channel.recv(r179)
        case _ <- recv(r179) => select {
        case _ <- recv(r178) => ()
        case _ <- recv(r178) => ()
        } ; Channel.recv(r180) ; ()
        } } ; spawn { select {
        case _ <- recv(r178) => Channel.recv(r179) ; Channel.recv(r180) ; ()
        case _ <- recv(r180) => Channel.recv(r178) ; Channel.recv(r179) ; ()
        } } ; ()
        } } ; spawn { select {
        case _ <- recv(r14) => Channel.recv(r15) ; Channel.recv(r12) ; Channel.recv(r16) ; Channel.recv(r13) ; ()
        case _ <- recv(r13) => Channel.recv(r12) ; Channel.recv(r14) ; Channel.recv(r15) ; Channel.recv(r16) ; ()
        } } ;
        ()
    }

    @test
    def testSelectRandom08(): Unit \ IO = {
        let (s15, r15) = Channel.buffered(Static, 0);
        let (s16, r16) = Channel.buffered(Static, 0);
        let (s17, r17) = Channel.buffered(Static, 0);
        let (s18, r18) = Channel.buffered(Static, 0);
        let (s19, r19) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s19) ; () } ; spawn { Channel.send((), s18) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s19) ; () } ; spawn { Channel.send((), s18) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s19) ; () } ; spawn { Channel.send((), s18) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { Channel.send((), s19) ; () } ; spawn { Channel.send((), s18) ; () } ; spawn { Channel.send((), s17) ; () } ; spawn { Channel.send((), s16) ; () } ; spawn { Channel.send((), s15) ; () } ; spawn { select {
        case _ <- recv(r15) => Channel.recv(r17) ; Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r19) ; ()
        } } ; spawn { select {
        case _ <- recv(r15) => Channel.recv(r19) ; Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r17) ; ()
        case _ <- recv(r18) => Channel.recv(r19) ; Channel.recv(r15) ; Channel.recv(r16) ; Channel.recv(r17) ; ()
        case _ <- recv(r19) => Channel.recv(r16) ; Channel.recv(r17) ; Channel.recv(r18) ; Channel.recv(r15) ; ()
        case _ <- recv(r17) => Channel.recv(r15) ; Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r19) ; ()
        } } ; spawn { select {
        case _ <- recv(r17) => Channel.recv(r18) ; Channel.recv(r15) ; Channel.recv(r16) ; Channel.recv(r19) ; ()
        case _ <- recv(r15) => Channel.recv(r17) ; Channel.recv(r16) ; Channel.recv(r18) ; Channel.recv(r19) ; ()
        case _ <- recv(r16) => Channel.recv(r15) ; Channel.recv(r18) ; Channel.recv(r17) ; Channel.recv(r19) ; ()
        case _ <- recv(r19) => Channel.recv(r15) ; Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r17) ; ()
        } } ; spawn { select {
        case _ <- recv(r17) => Channel.recv(r18) ; Channel.recv(r16) ; Channel.recv(r19) ; Channel.recv(r15) ; ()
        case _ <- recv(r16) => Channel.recv(r18) ; select {
        case _ <- recv(r15) => ()
        case _ <- recv(r15) => ()
        } ; Channel.recv(r17) ; Channel.recv(r19) ; let (s214, r214) = Channel.buffered(Static, 0);
        let (s215, r215) = Channel.buffered(Static, 0);
        let (s216, r216) = Channel.buffered(Static, 0);
        let (s217, r217) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s217) ; () } ; spawn { Channel.send((), s216) ; () } ; spawn { Channel.send((), s215) ; () } ; spawn { Channel.send((), s214) ; () } ; select {
        case _ <- recv(r215) => Channel.recv(r214) ; Channel.recv(r216) ; Channel.recv(r217) ; ()
        case _ <- recv(r214) => Channel.recv(r216) ; Channel.recv(r215) ; Channel.recv(r217) ; ()
        case _ <- recv(r216) => Channel.recv(r214) ; Channel.recv(r217) ; Channel.recv(r215)
        }
        } } ;
        ()
    }

    @test
    def testSelectRandom09(): Unit \ IO = {
        let (s6, r6) = Channel.buffered(Static, 0);
        let (s7, r7) = Channel.buffered(Static, 0);
        let (s8, r8) = Channel.buffered(Static, 0);
        let (s9, r9) = Channel.buffered(Static, 0);
        spawn { select {
        case _ <- recv(r8) => ()
        case _ <- recv(r8) => ()
        } ; Channel.send((), s9) ; () } ; spawn { Channel.recv(r7) ; Channel.send((), s8) ; () } ; spawn { select {
        case _ <- recv(r6) => ()
        case _ <- recv(r6) => ()
        } ; Channel.send((), s7) ; () } ; Channel.send((), s6) ; Channel.recv(r9);
        ()
    }

    @test
    def testSelectRandom10(): Unit \ IO = {
        let (s10, r10) = Channel.buffered(Static, 0);
        let (s11, r11) = Channel.buffered(Static, 0);
        let (s12, r12) = Channel.buffered(Static, 0);
        let (s13, r13) = Channel.buffered(Static, 0);
        spawn { Channel.send((), s13) ; () } ;
        spawn { Channel.send((), s12) ; () } ;
        spawn { Channel.send((), s11) ; () } ;
        spawn { Channel.send((), s10) ; () } ;
        select {
            case _ <- recv(r10) => Channel.recv(r13) ; Channel.recv(r12) ; Channel.recv(r11)
            case _ <- recv(r13) => Channel.recv(r12) ; Channel.recv(r10) ; Channel.recv(r11)
            case _ <- recv(r11) => Channel.recv(r13) ; Channel.recv(r12) ; Channel.recv(r10)
        };
        ()
    }

    @test
    def testSelectSideEffecting01(): Bool \ IO = {
        def mkChan(): Channel[Int32] \ IO = {
            let (s, r) = Channel.buffered(Static, 1);
            Channel.send(42, s);
            r
        };

        select {
            case x <- recv(mkChan()) => x == 42
        }
    }

    @test
    def testSelectSideEffecting02(): Bool \ IO = {
        let (s1, r1) = Channel.buffered(Static, 10);
        let (s2, r2) = Channel.buffered(Static, 10);
        let (s3, r3) = Channel.buffered(Static, 10);

        select {
            case x <- recv({Channel.send(1, s3); r1}) => x + (Channel.recv(r2)) + (Channel.recv(r3)) == 6
            case x <- recv({Channel.send(2, s2); r2}) => x + (Channel.recv(r1)) + (Channel.recv(r3)) == 6
            case x <- recv({Channel.send(3, s1); r3}) => x + (Channel.recv(r1)) + (Channel.recv(r2)) == 6
        }
    }

    type alias MyReceiver = Receiver[Int32, Static]

    @test
    def testSelectAliasedChannel(): Bool \ IO = {
        let (s, r) = Channel.buffered(Static, 1);

        def useChan(mr: MyReceiver) : Bool =
            select {
                case x <- recv(mr) => x == 42
            };

        Channel.send(42, s);
        useChan(r)
    }

    @test
    def testSelectOptionalSyntax01(): Bool \ IO = 
        let (s1, r1) = Channel.buffered(Static, 1);
        spawn Channel.send(1, s1);
        select {
            case x <- Channel.recv(r1) => x == 1
        }
}
