use Net.Http
use Net.Response

/// Sends a GET request over HTTP and prints the response body.
/// The Http effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Http, IO } =
    match Http.get("http://example.com/") {
        case Ok(resp) => println(Response.body(resp))
        case Err(err) => println(err)
    }
