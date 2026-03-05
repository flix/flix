use Net.Https
use Net.Response

/// Sends a GET request over HTTPS and prints the response body.
/// The Https handler ensures that the connect is secure.
/// The Https effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Https, IO } =
    match Https.get("https://example.com/") {
        case Ok(resp) => println(Response.body(resp))
        case Err(err) => println(err)
    }
