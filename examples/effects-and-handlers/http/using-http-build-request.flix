use Net.Http
use Net.Request
use Net.Response
use Net.Timeout

/// Demonstrates building an HTTP request with the `Request` API.
///
/// Uses a fluent pipeline to add query parameters, a bearer token,
/// a custom header, and a timeout before sending the request with
/// `Http.send`. The response status and a truncated body are printed.
def main(): Unit \ IO =
    run {
        let req = Request.get("https://flix.dev/api/search")
                    |> Request.withQueryParam("q", "flix lang")
                    |> Request.withQueryParams(Map#{"page" => "1", "limit" => "10"})
                    |> Request.withBearerToken("my-secret-token")
                    |> Request.withHeader("X-Custom", "hello")
                    |> Request.withTimeout(Timeout.After(5, TimeUnit.Seconds));
        match Http.send(req) {
            case Ok(resp) =>
                println("Status: ${Response.status(resp)}");
                println("Body: ${String.takeLeft(80, Response.body(resp))}")
            case Err(err) =>
                println("Error: ${err}")
        }
    } with Http.runWithIO
