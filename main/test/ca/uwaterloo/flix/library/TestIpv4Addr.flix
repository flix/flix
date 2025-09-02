mod TestIpv4Addr {

    use Ipv4Addr.Ipv4Addr;
    use Assert.{assertEq, assertNone};

    @Test
    def fromString01(): Unit \ Assert =
        assertNone(Ipv4Addr.fromString("256.100.50.25"))

    @Test
    def fromString02(): Unit \ Assert =
        assertNone(Ipv4Addr.fromString("192.168.1"))

    @Test
    def fromString03(): Unit \ Assert =
        assertNone(Ipv4Addr.fromString("123.45.67.89.1"))

    @Test
    def fromString04(): Unit \ Assert =
        assertNone(Ipv4Addr.fromString("123.045.067.089"))

    @Test
    def fromString05(): Unit \ Assert =
        assertNone(Ipv4Addr.fromString("192.168.1.300"))

    @Test
    def fromString06(): Unit \ Assert =
        let s = "192.168.1.1";
        assertEq(expected = "Some(${s})", "${Ipv4Addr.fromString(s)}")

    @Test
    def fromString07(): Unit \ Assert =
        let s = "8.8.8.8";
        assertEq(expected = "Some(${s})", "${Ipv4Addr.fromString(s)}")

    @Test
    def fromString08(): Unit \ Assert =
        let s = "10.0.0.255";
        assertEq(expected = "Some(${s})", "${Ipv4Addr.fromString(s)}")

    @Test
    def fromString09(): Unit \ Assert =
        let s = "172.16.254.3";
        assertEq(expected = "Some(${s})", "${Ipv4Addr.fromString(s)}")

    @Test
    def fromString10(): Unit \ Assert =
        let s = "1.2.3.4";
        assertEq(expected = "Some(${s})", "${Ipv4Addr.fromString(s)}")

    @Test
    def localhost01(): Unit \ Assert =
        assertEq(expected = "127.0.0.1", "${Ipv4Addr.localhost()}")

}
