mod TestSocketAddr {

    use Assert.{assertEq, assertNone, fail};

    @Test
    def fromString01(): Unit \ Assert =
        match SocketAddr.fromString("192.168.1.1:8080") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                assertEq(expected = 8080, port);
                assertEq(expected = "V4(192.168.1.1)", "${ip}")
            }
            case None => fail("Expected valid socket address")
        }

    @Test
    def fromString02(): Unit \ Assert =
        match SocketAddr.fromString("[2001:db8::1]:443") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                assertEq(expected = 443, port);
                assertEq(expected = "V6(2001:0DB8:0000:0000:0000:0000:0000:0001)", "${ip}")
            }
            case None => fail("Expected valid socket address")
        }

    @Test
    def fromString03(): Unit \ Assert =
        match SocketAddr.fromString("10.0.0.1:53") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                assertEq(expected = 53, port);
                assertEq(expected = "V4(10.0.0.1)", "${ip}")
            }
            case None => fail("Expected valid socket address")
        }

    @Test
    def fromString04(): Unit \ Assert =
        match SocketAddr.fromString("[::1]:8080") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                assertEq(expected = 8080, port);
                assertEq(expected = "V6(0000:0000:0000:0000:0000:0000:0000:0001)", "${ip}")
            }
            case None => fail("Expected valid socket address")
        }

    @Test
    def fromString05(): Unit \ Assert =
        match SocketAddr.fromString("[2001:0db8:85a3::8a2e:0370:7334]:65535") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                assertEq(expected = 65535, port);
                assertEq(expected = "V6(2001:0DB8:85A3:0000:0000:8A2E:0370:7334)", "${ip}")
            }
            case None => fail("Expected valid socket address")
        }

    @Test
    def fromString06(): Unit \ Assert =
        assertNone(SocketAddr.fromString("256.100.50.25:8080"))

    @Test
    def fromString07(): Unit \ Assert =
        assertNone(SocketAddr.fromString("156.100.50.25:80808080"))

    @Test
    def fromString08(): Unit \ Assert =
        assertNone(SocketAddr.fromString("156.100.50.25:65536"))

    @Test
    def fromString09(): Unit \ Assert =
        assertNone(SocketAddr.fromString("[:1:8080"))

    @Test
    def fromString10(): Unit \ Assert =
        assertNone(SocketAddr.fromString("0::1]:8080"))

    @Test
    def fromString11(): Unit \ Assert =
        assertNone(SocketAddr.fromString("[2345:0425:2CA1:0000:0000:0567:5673:23B5:1234]:8080"))

}

