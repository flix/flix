use Net.Http
use Net.Retry
use Net.Response

/// Demonstrates composing multiple middleware via `with` clauses.
/// The innermost handler (first `with`) intercepts the request first,
/// then re-raises it to the next handler, and so on until `runWithIO`
/// performs the actual I/O.
///
/// This example stacks: base URL, default headers, logging, retry,
/// circuit breaker, and sliding-window rate limiting.
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
