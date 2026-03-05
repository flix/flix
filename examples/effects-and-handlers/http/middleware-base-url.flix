use Net.Http
use Net.Response

/// Uses `withBaseUrl` to set a common prefix for request URLs.
/// Relative paths (without "://") are prefixed with the base URL.
/// Absolute URLs bypass the base and are sent as-is.
def main(): Unit \ IO =
    run {
        // Relative paths are prefixed with the base URL.
        match Http.get("/api/users") {
            case Ok(resp) => println("/api/users -> ${Response.status(resp)}")
            case Err(err) => println("/api/users -> ${err}")
        };
        match Http.get("/api/posts") {
            case Ok(resp) => println("/api/posts -> ${Response.status(resp)}")
            case Err(err) => println("/api/posts -> ${err}")
        };
        // Absolute URLs bypass the base.
        match Http.get("https://flix.dev/other") {
            case Ok(resp) => println("absolute   -> ${Response.status(resp)}")
            case Err(err) => println("absolute   -> ${err}")
        }
    } with Http.withBaseUrl("https://flix.dev")
      with Http.runWithIO
