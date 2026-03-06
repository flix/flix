use Net.Http
use Net.Response

/// Uses `withCircuitBreaker` to protect against cascading failures.
/// After `failureThreshold` consecutive failures (transport errors or 5xx),
/// the circuit opens and rejects requests for `cooldownMs` milliseconds.
/// A successful request resets the failure counter.
def main(): Unit \ { Clock, Http, IO } =
    run {
        let urls = List#{"/a", "/b", "/c", "/d", "/e", "/f", "/g", "/h"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }
    } with Http.withCircuitBreaker(failureThreshold = 3, cooldownMs = 5000)
      with Http.withBaseUrl("https://notfound.flix.dev")
