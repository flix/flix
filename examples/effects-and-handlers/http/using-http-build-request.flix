use Net.Http
use Net.Request
use Net.Response
use Net.Timeout

/// Demonstrates building an HTTP request with the `Request` API.
/// Uses a fluent pipeline to add query parameters, a bearer token,
/// a custom header, and a timeout before sending with `Http.send`.
def main(): Unit \ { Http, IO } =
    let req = Request.get("https://flix.dev/api/search")
                |> Request.withQueryParam("q", "flix programming language")
                |> Request.withQueryParams(Map#{"page" => "1", "per_page" => "25", "sort" => "relevance"})
                |> Request.withBearerToken("ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789")
                |> Request.withHeader("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
                |> Request.withTimeout(Timeout.After(5, TimeUnit.Seconds));
    match Http.send(req) {
        case Ok(resp) =>
            println("Status: ${Response.status(resp)}");
            println("Body: ${String.takeLeft(80, Response.body(resp))}")
        case Err(err) =>
            println("Error: ${err}")
    }
