use Net.Http
use Net.Response

/// Uses `withLogging` to log each request and response via the `Logger` effect.
def main(): Unit \ { Http, Logger, IO } =
    run {
        match Http.get("https://flix.dev/") {
            case Ok(resp) => println("Status: ${Response.status(resp)}")
            case Err(err) => println("Error: ${err}")
        };
        match Http.get("https://notfound.flix.dev/") {
            case Ok(resp) => println("Status: ${Response.status(resp)}")
            case Err(err) => println("Error: ${err}")
        }
    } with Http.withLogging
