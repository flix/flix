use Assert.{assertEq, assertTrue, fail}
    use Concurrent.Channel.Mpmc
    use Concurrent.Channel.unsafeGetAndUnlock
    use Concurrent.Channel.get
    use Concurrent.Channel.mpmcAdmin
    use Concurrent.Channel.newChannel
    use Concurrent.Channel.put
    use Concurrent.Channel.selectFrom

    // pub def main(): Unit \ Assert + IO =
    //     let c = newChannel(1);
    //     put(2, c);
    //     match selectFrom(mpmcAdmin(c) :: Nil, true) {
    //         case (0, locks) =>
    //             let i = unsafeGetAndUnlock(c, locks);
    //             assertEq(expected = 2, i)
    //         case _ => unreachable!()
    //     }

mod TestChannel {

    use Assert.{assertEq, assertTrue, fail}
    use Concurrent.Channel.Mpmc
    use Concurrent.Channel.unsafeGetAndUnlock
    use Concurrent.Channel.get
    use Concurrent.Channel.mpmcAdmin
    use Concurrent.Channel.newChannel
    use Concurrent.Channel.put
    use Concurrent.Channel.selectFrom


    @Test
    def getPut01(): Unit \ Assert + IO =
        let c = newChannel(1);
        put(true, c);
        assertTrue(get(c))

    @Test
    def getPut02(): Unit \ Assert + IO =
        let c = newChannel(1);
        put(123, c);
        assertEq(expected = 123, get(c))

    @Test
    def getPut03(): Unit \ Assert + IO =
        let c = newChannel(1);
        put("Hello World!", c);
        assertEq(expected = "Hello World!", get(c))

    pub def main(): Unit \ Assert + IO =
        let c = newChannel(1);
        put(2, c);
        match selectFrom(mpmcAdmin(c) :: Nil, true) {
            case (0, locks) =>
                let i = unsafeGetAndUnlock(c, locks);
                assertEq(expected = 3, i)
            case _ => unreachable!()
        }

    @Test
    def select02(): Unit \ Assert + IO =
        let c = newChannel(1);
        match selectFrom(mpmcAdmin(c) :: Nil, false) {
            case (0, locks) =>
                let _ = unsafeGetAndUnlock(c, locks);
                fail("Should not have received from empty channel")
            case (-1, _) =>
                () // Success - no channels were ready
            case _ => unreachable!()
        }

    @Test
    def select03(): Unit \ Assert + IO =
        let c1: Mpmc[Int32, Static] = newChannel(0);
        let c2: Mpmc[String, _] = newChannel(10);

        put("hey", c2);

        match selectFrom(mpmcAdmin(c1) :: mpmcAdmin(c2) :: Nil, true) {
            case (0, locks) =>
                let _ = unsafeGetAndUnlock(c1, locks);
                bug!("The channel should be empty")
            case (1, locks) =>
                let s = unsafeGetAndUnlock(c2, locks);
                assertEq(expected = "hey", s)
            case _ => unreachable!()
        }

}
