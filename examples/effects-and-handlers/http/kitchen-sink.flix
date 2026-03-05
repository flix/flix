use Net.Http
use Net.Request
use Net.Response
use Net.Retry
use Net.Timeout

/// A comprehensive example exercising most of the HTTP client API surface:
/// custom request building, response inspection, and the full middleware stack.
def main(): Unit \ IO =
    let defaultHeaders = Map#{
        "Accept"     => List#{"application/json"},
        "User-Agent" => List#{"flix-http/1.0"}
    };
    run {
        // Build a POST request with JSON body, bearer token, and a timeout.
        let req = Request.post("/api/users", "{\"name\": \"Alice\"}")
            |> Request.withContentType("application/json")
            |> Request.withBearerToken("my-secret-token")
            |> Request.withHeader("X-Request-Id", "abc-123")
            |> Request.withQueryParam("notify", "true")
            |> Request.withTimeout(Timeout.After(10, TimeUnit.Seconds));

        // Send the request and inspect the response.
        match Http.send(req) {
            case Ok(resp) =>
                println("Status:         ${Response.status(resp)}");
                println("Is success?     ${Response.isSuccess(resp)}");
                println("Content-Type:   ${Response.contentType(resp)}");
                println("Content-Length: ${Response.contentLength(resp)}");
                println("Server:         ${Response.headerValue("server", resp)}");
                println("Body:           ${String.takeLeft(80, Response.body(resp))}")
            case Err(err) =>
                println("Error: ${err}")
        };

        // Also do a simple GET to show the base URL and default headers at work.
        match Http.get("/api/posts") {
            case Ok(resp) => println("/api/posts -> ${Response.status(resp)}")
            case Err(err) => println("/api/posts -> ${err}")
        }
    } with Http.withBaseUrl("http://example.com")             // prepend base URL to relative paths
      with Http.withDefaultHeaders(defaultHeaders)            // inject Accept and User-Agent into every request
      with Http.withLogging                                   // log each request/response via the Logger effect
      with Http.withRetry(Retry.exponential(3, 200i64))       // retry up to 3 times with exponential backoff
      with Http.withCircuitBreaker(5, 10_000i64)              // open circuit after 5 consecutive failures for 10s
      with Http.withSlidingWindow(4, 2000i64)                 // allow at most 4 requests per 2s sliding window
      with Http.runWithIO                                     // execute actual HTTP I/O
      with Logger.runWithIO                                   // execute logger I/O
