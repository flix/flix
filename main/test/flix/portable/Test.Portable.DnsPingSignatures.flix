mod Test.Portable.DnsPingSignatures {

    // Compile-only signatures (not executed) to ensure these effects are present
    // in the portable stdlib profile.

    def dnsLookupSig(host: String): IpAddr \ Dns =
        Dns.lookup(host)

    def dnsLookupAllSig(host: String): List[IpAddr] \ Dns =
        Dns.lookupAll(host)

    def dnsWithResultLookupSig(host: String): Result[IoError, IpAddr] \ DnsWithResult =
        DnsWithResult.lookup(host)

    def pingSig(host: IpAddr): Bool \ Ping =
        Ping.isReachable(host, 1, TimeUnit.Seconds)

    def pingWithResultSig(host: IpAddr): Result[IoError, Bool] \ PingWithResult =
        PingWithResult.isReachable(host, 1, TimeUnit.Seconds)

    @Test
    def smoke01(): Unit = {
        let _ = dnsLookupSig;
        let _ = dnsLookupAllSig;
        let _ = dnsWithResultLookupSig;
        let _ = pingSig;
        let _ = pingWithResultSig;
        ()
    }

    pub def runAll(): Unit = {
        let _ = smoke01();
        ()
    }

}
