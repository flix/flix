mod TestDns {
    use Assert.assertEq
    
    //Testing the translation of Dns -> DnsWithResult
    //with stubbed behavior for DnsWithResult
    @Test
    def runWithDnsWithResult01(): Unit \ Assert =
        //stub ip
        let stubIPOpt = IpAddr.fromString("0.0.0.0");
        let stubIp = match stubIPOpt {
                case Some(x) => x
                case None => unreachable!()
        };

        let result = run {
            Dns.lookup("stubbed behavior")
        } with runWithDnsWithResult //transform!
        with handler DnsWithResult {
            def lookup(host, k) = {
                k(Ok(stubIp))
            }
            def lookupAll(host, k) = ???
        };
        assertEq(expected = Ok(stubIp), result)
}