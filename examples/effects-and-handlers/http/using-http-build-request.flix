use Net.Http
use Net.HttpRequest
use Net.HttpResponse
use Time.Duration.seconds

/// Demonstrates building an HTTP request with the `HttpRequest` API.
/// Uses a fluent pipeline to add query parameters, a bearer token,
/// a custom header, and a timeout before sending with `Http.send`.
def main(): Unit \ { Http, IO } =
    let req = HttpRequest.get("https://flix.dev/api/search")
                |> HttpRequest.withQueryParam("q", "flix programming language")
                |> HttpRequest.withQueryParams(Map#{"page" => "1", "per_page" => "25", "sort" => "relevance"})
                |> HttpRequest.withBearerToken("ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789")
                |> HttpRequest.withHeader("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
                |> HttpRequest.withTimeout(seconds(5));
    match Http.send(req) {
        case Ok(resp) =>
            println("Status: ${HttpResponse.status(resp)}");
            println("Body: ${String.takeLeft(80, HttpResponse.body(resp))}")
        case Err(err) =>
            println("Error: ${err}")
    }
