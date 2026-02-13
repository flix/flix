mod TestDnsWithResult {
    use Assert.assertEq
    use DnsWithResult.{lookup, runWithDns}

    //Testing the translation of DnsWithResult -> Dns
    //with stubbed behavior for Dns
    @Test
    def runWithDns01(): Unit \ Assert =
        //stub ip
        let stubIPOpt = IpAddr.fromString("0.0.0.0");
        let stubIp = match stubIPOpt {
                case Some(x) => x
                case None => unreachable!()
        };
        let result = run {
            DnsWithResult.lookup("To be stubbed")
        } with runWithDns //transform!
        with handler Dns {
            def lookup(host, k) = k(stubIp)
            def lookupAll(host, k) = ???
        };
        assertEq(expected = stubIp, result)
}