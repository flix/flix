use Net.Http
use Net.Retry
use Net.Response

/// Demonstrates composing multiple middleware via `with` clauses.
/// Middleware is applied bottom-to-top: `runWithIO` executes the actual I/O,
/// then each layer wraps the one below it.
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
        List.forEach(url -> {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${Response.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        }, urls)
    } with Http.withBaseUrl("https://flix.dev")
      with Http.withDefaultHeaders(defaultHeaders)
      with Http.withLogging
      with Http.withRetry(Retry.retryTransportOnly(2, 500i64))
      with Http.withCircuitBreaker(3, 5000i64)
      with Http.withSlidingWindow(2, 1000i64)
      with Http.runWithIO
      with Logger.runWithIO
