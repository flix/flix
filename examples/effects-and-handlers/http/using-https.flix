use Net.Https
use Net.Response

/// Sends a GET request over HTTPS and prints the response body.
/// The Https effect enforces a secure connection — only https URLs are allowed.
/// Unlike the Http effect, which supports both http and https URLs, Https rejects non-secure URLs.
/// The Https effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Https, IO } =
    match Https.get("https://example.com/") {
        case Ok(resp) => println(Response.body(resp))
        case Err(err) => println(err)
    }
