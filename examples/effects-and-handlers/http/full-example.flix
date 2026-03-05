use Net.Http
use Net.Retry
use Net.Response

/// Demonstrates composing multiple HTTP middleware via `with` clauses.
///
/// Each `with` wraps the preceding block, so the last handler listed
/// (`runWithIO`) is the outermost — it performs the actual I/O.
/// A request travels outward through the stack: `withBaseUrl` prepends
/// the base URL, `withDefaultHeaders` merges in default headers,
/// `withRetry` adds automatic retries with a linear back-off,
/// `withCircuitBreaker` short-circuits after repeated failures,
/// `withSlidingWindow` rate-limits requests, and `withLogging` logs
/// each request/response via the `Logger` effect.
///
/// The example sends GET requests to relative paths (resolved against
/// `https://flix.dev`) and to an absolute URL that bypasses the base URL.
def main(): Unit \ {Clock, IO} =
    let defaultHeaders = Map#{
        "Accept"        => List#{"application/json"},
        "Authorization" => List#{"Bearer tok123"}
    };
    run {
        let urls = List#{"/api/users", "/api/posts"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        };
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("notfound -> ${Response.status(resp)}")
            case Err(err) => println("notfound -> ${err}")
        }
    } with Http.withBaseUrl("https://flix.dev")
      with Http.withDefaultHeaders(defaultHeaders)
      with Http.withRetry(Retry.linear(2, 100i64))
      with Http.withCircuitBreaker(3, 5000i64)
      with Http.withSlidingWindow(2, 1000i64)
      with Http.withLogging
      with Http.runWithIO
      with Logger.runWithIO
