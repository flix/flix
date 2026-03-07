use Net.Http
use Net.HttpResponse

/// Sends a GET request over HTTP and prints the response body.
/// The Http effect supports both http and https URLs.
/// For enforced secure connections, use the Https effect instead.
/// The Http effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Http, IO } =
    match Http.get("http://example.com/") {
        case Ok(resp) => println(HttpResponse.body(resp))
        case Err(err) => println(err)
    }
