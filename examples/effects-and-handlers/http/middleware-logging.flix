use Net.Http
use Net.Response

/// Uses `withLogging` to log each request and response via the `Logger` effect.
/// Requires a `Logger` handler (here `Logger.runWithIO`) to print the log lines.
def main(): Unit \ IO =
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
      with Http.runWithIO
      with Logger.runWithIO
