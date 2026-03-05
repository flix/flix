use Net.Http
use Net.Response

/// Uses `withCircuitBreaker` to protect against cascading failures.
/// After `failureThreshold` consecutive failures (transport errors or 5xx),
/// the circuit opens and rejects requests for `cooldownMs` milliseconds.
/// A successful request resets the failure counter.
def main(): Unit \ IO =
    run {
        let urls = List#{"/a", "/b", "/c", "/d"};
        List.forEach(url -> {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }, urls)
    } with Http.withCircuitBreaker(3, 5000i64)
      with Http.withBaseUrl("http://example.com")
      with Http.runWithIO
