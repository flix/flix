use Net.Http
use Net.Response

/// Uses `withDefaultHeaders` to inject headers into every request.
/// Headers already present on a request are not overridden.
def main(): Unit \ { Http, IO } =
    let defaults = Map#{
        "Accept"        => List#{"application/json"},
        "Authorization" => List#{"Bearer my-token"}
    };
    run {
        match Http.get("https://flix.dev/api/users") {
            case Ok(resp) => println("Status: ${Response.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withDefaultHeaders(defaults)
