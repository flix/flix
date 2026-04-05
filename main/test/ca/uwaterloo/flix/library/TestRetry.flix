//todo: fix this so its a module, but for draft its just "draft code"


@Test
def test01(): Unit \ IO = 
    let pol = Retry.limitRetries(100);
    let res = run {
        Net.Dns.lookup("Somesitehere.nothing")
    } with dnsHandleWithRetry(pol)
    with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)

@Test
def test02(): Unit \ IO =
    let pol = Retry.combine(Retry.expBackoff(), Retry.limitRetries(5));
    let res = run {
        Net.Dns.lookup("Somesitehere.nothing")
    } with dnsHandleWithRetry(pol) 
    with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)

@Test
def test03(): Unit \ IO =
    let pol = Retry.combine(Retry.constantBackoff(Time.Duration.milliseconds(5)), Retry.until(() -> false));
    let res = run {
        Net.Dns.lookup("Somesitehere.nothing")
    } with dnsHandleWithRetry(pol) 
    with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)

//manually using the retry "engine"
@Test
def test04(): Unit \ IO =
    let retry = Retry.retry(Retry.combine(Retry.constantBackoff(Time.Duration.milliseconds(5)), Retry.until(() -> false)));
    let res = run {
        retry(Net.Dns.lookup)("Somesitehere.nothing")
    } with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)

@Test
def test05(): Unit \ IO + Math.Random =
    let pol = Retry.constantBackoff(Time.Duration.milliseconds(5)) |> Retry.addJitter(Time.Duration.nanoseconds(400i64));
    let res = run {
        Net.Dns.lookup("Somesitehere.nothing")
    } with dnsHandleWithRetry(pol)
    with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)

@Test
def test06(): Unit \ IO + Time.Clock =
    let startTime = Time.Clock.now();
    let pol = Retry.combine(Retry.deadline(startTime, Time.Duration.milliseconds(500)), Retry.constantBackoff(Time.Duration.milliseconds(5)));
    let res = run {
        Net.Dns.lookup("Somesitehere.nothing")
    } with dnsHandleWithRetry(pol)
    with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)

// ---------------------------- effect test ----------------------------
@Test
def test07(): Unit \ IO + Time.Clock=
    let pol = Retry.combine(Retry.constantBackoff(Time.Duration.milliseconds(5)), Retry.deadline(Time.Clock.now(), Time.Duration.milliseconds(3000)));
    let res = run {
        Net.Dns.lookup("Somesitehere.nothing")
    } with dnsHandleWithRetry2
    with Retry.handle(pol)
    with Net.Dns.runWithIO
    with Time.Sleep.runWithIO
    with Sys.Console.runWithIO;
    println(res)