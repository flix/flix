def mismatchedClockAndHttp(): Unit \ { Time.Clock } =
    let u = "http://example.com/";
    discard Net.Http.get(u)
