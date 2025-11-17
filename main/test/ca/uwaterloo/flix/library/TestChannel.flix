mod TestChannel {

    use Assert.{assertEq, assertTrue, fail}
    use Concurrent.Channel.Mpmc
    use Concurrent.Channel.unsafeGetAndUnlock
    use Concurrent.Channel.get
    use Concurrent.Channel.mpmcAdmin
    use Concurrent.Channel.newChannelTuple
    use Concurrent.Channel.put
    use Concurrent.Channel.selectFrom
    use Concurrent.Channel.Rec
    use Concurrent.Channel.Sen

    @Test
    def getPut01(): Unit \ Assert + IO =
        let (c_s, c_r) = newChannelTuple(1);
        put(true, c_s);
        assertTrue(get(c_r))

    @Test
    def getPut02(): Unit \ Assert + IO =
        let (c_s, c_r) = newChannelTuple(1);
        put(123, c_s);
        assertEq(expected = 123, get(c_r))

    @Test
    def getPut03(): Unit \ Assert + IO =
        let (c_s, c_r) = newChannelTuple(1);
        put("Hello World!", c_s);
        assertEq(expected = "Hello World!", get(c_r))

    @Test
    def select01(): Unit \ Assert + IO =
        let (c_s, c_r) = newChannelTuple(1);
        put(2, c_s);
        match selectFrom(mpmcAdmin(c_r) :: Nil, true) {
            case (0, locks) =>
                let i = unsafeGetAndUnlock(c_r, locks);
                assertEq(expected = 2, i)
            case _ => unreachable!()
        }

    @Test
    def select02(): Unit \ Assert + IO =
        let (_, c_r) = newChannelTuple(1);
        match selectFrom(mpmcAdmin(c_r) :: Nil, false) {
            case (0, locks) =>
                let _ = unsafeGetAndUnlock(c_r, locks);
                fail("Should not have received from empty channel")
            case (-1, _) =>
                () // Success - no channels were ready
            case _ => unreachable!()
        }

    @Test
    def select03(): Unit \ Assert + IO =
        let (_, c1_r): (Mpmc[Int32, Sen, Static], Mpmc[Int32, Rec, Static]) = newChannelTuple(0);
        let (c2_s, c2_r): (Mpmc[String, Sen, Static], Mpmc[String, Rec, Static]) = newChannelTuple(10);

        put("hey", c2_s);

        match selectFrom(mpmcAdmin(c1_r) :: mpmcAdmin(c2_r) :: Nil, true) {
            case (0, locks) =>
                let _ = unsafeGetAndUnlock(c1_r, locks);
                bug!("The channel should be empty")
            case (1, locks) =>
                let s = unsafeGetAndUnlock(c2_r, locks);
                assertEq(expected = "hey", s)
            case _ => unreachable!()
        }

}
