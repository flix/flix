use Net.Http
use Net.HttpResponse

/// Uses `withDefaultHeaders` to inject headers into every request.
/// Headers already present on a request are not overridden.
def main(): Unit \ { Http, IO } =
    let defaults = Map#{
        "Accept"        => List#{"application/json"},
        "Authorization" => List#{"Bearer ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789"}
    };
    run {
        match Http.get("https://flix.dev/api/users") {
            case Ok(resp) => println("Status: ${HttpResponse.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withDefaultHeaders(defaults)
