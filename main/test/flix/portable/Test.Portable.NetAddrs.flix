mod Test.Portable.NetAddrs {

    @Test
    def ipv4FromString01(): Unit =
        match Ipv4Addr.fromString("192.168.1.1") {
            case Some(_) => ()
            case None => bug!("expected ipv4 parse to succeed.")
        }

    @Test
    def ipv6FromString01(): Unit =
        match Ipv6Addr.fromString("FE80::1234") {
            case Some(addr) =>
                if (ToString.toString(addr) == "FE80:0000:0000:0000:0000:0000:0000:1234") ()
                else bug!("unexpected ipv6 formatting.")
            case None => bug!("expected ipv6 parse to succeed.")
        }

    @Test
    def socketAddrFromString01(): Unit =
        match SocketAddr.fromString("[::1]:8080") {
            case Some(_) => ()
            case None => bug!("expected socket addr parse to succeed.")
        }

    pub def runAll(): Unit = {
        let _ = ipv4FromString01();
        let _ = ipv6FromString01();
        let _ = socketAddrFromString01();
        ()
    }

}
