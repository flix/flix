use Net.Http
use Net.Response

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
        match Http.get("http://example.com/other") {
            case Ok(resp) => println("absolute   -> ${Response.status(resp)}")
            case Err(err) => println("absolute   -> ${err}")
        }
    } with Http.withBaseUrl("http://example.com")
      with Http.runWithIO
