mod TestIpv6Addr {

    use Ipv6Addr.Ipv6Addr;
    use Assert.{assertEq, assertNone};

    @Test
    def fromString01(): Unit \ Assert =
        assertNone(Ipv6Addr.fromString("192.168.1.1"))

    @Test
    def fromString02(): Unit \ Assert =
        assertNone(Ipv6Addr.fromString("2345:0425:2CA1:0000:0000:0567:5673:23G5"))

    @Test
    def fromString03(): Unit \ Assert =
        assertNone(Ipv6Addr.fromString("2345:0425:2CA1:0000:0000:0567:5673:23B5:1234"))

    @Test
    def fromString04(): Unit \ Assert =
        assertNone(Ipv6Addr.fromString("192:168:1"))

    @Test
    def fromString05(): Unit \ Assert =
        assertNone(Ipv6Addr.fromString("2345:0425:2CA1:0567:5673:"))

    @Test
    def fromString06(): Unit \ Assert =
        let s = "2345:0425:2CA1:0000:0000:0567:5673:23B5";
        assertEq(expected = "Some(${s})", "${Ipv6Addr.fromString(s)}")

    @Test
    def fromString07(): Unit \ Assert =
        let s = "2345:0425:2CA1:0000:0000:0567:5673:23B5";
        assertEq(expected = "Some(${s})", "${Ipv6Addr.fromString(s)}")

    @Test
    def fromString08(): Unit \ Assert =
        let input = "2345:425:2CA1:0:0:567:5673:23B5";
        let expected = "2345:0425:2CA1:0000:0000:0567:5673:23B5";
        assertEq(expected = "Some(${expected})", "${Ipv6Addr.fromString(input)}")

    @Test
    def fromString09(): Unit \ Assert =
        let input = "::1234";
        let expected = "0000:0000:0000:0000:0000:0000:0000:1234";
        assertEq(expected = "Some(${expected})", "${Ipv6Addr.fromString(input)}")

    @Test
    def fromString10(): Unit \ Assert =
        let input = "FE80::1234";
        let expected = "FE80:0000:0000:0000:0000:0000:0000:1234";
        assertEq(expected = "Some(${expected})", "${Ipv6Addr.fromString(input)}")

    @Test
    def localhost01(): Unit \ Assert =
        assertEq(expected = "0000:0000:0000:0000:0000:0000:0000:0001", "${Ipv6Addr.localhost()}")

}
