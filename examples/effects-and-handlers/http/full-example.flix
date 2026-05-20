use Net.Http
use Net.Retry
use Net.HttpResponse
use Time.Clock
use Time.Duration.{milliseconds, seconds}

/// Demonstrates composing multiple HTTP middleware via `with` clauses.
/// Stacks base URL, default headers, retry, circuit breaker,
/// sliding-window rate limiting, and logging. Each `with` wraps the
/// preceding block. The `Http` and `Logger` effects propagate to `main`
/// and are handled automatically via their default handlers.
/// Relative paths are resolved against the base URL; absolute URLs bypass it.
def main(): Unit \ { Clock, Http, Logger, IO } =
    let defaultHeaders = Map#{
        "Accept"        => List#{"application/json"},
        "Authorization" => List#{"Bearer tok123"}
    };
    run {
        let urls = List#{"/api/users", "/api/posts"};
        foreach (url <- urls) {
            match Http.get(url) {
                case Ok(resp) => println("${url} -> ${HttpResponse.status(resp)}")
                case Err(err) => println("${url} -> ${err}")
            }
        };
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("notfound -> ${HttpResponse.status(resp)}")
            case Err(err) => println("notfound -> ${err}")
        }
    } with Http.withBaseUrl("https://flix.dev")
      with Http.withDefaultHeaders(defaultHeaders)
      with Http.withRetry(Retry.linear(maxRetries = 2, delay = milliseconds(100)))
      with Http.withCircuitBreaker(failureThreshold = 3, cooldown = seconds(5))
      with Http.withSlidingWindow(maxRequests = 2, window = seconds(1))
      with Http.withLogging
