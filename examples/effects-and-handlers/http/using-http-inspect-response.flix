use Net.Http
use Net.Response

/// Demonstrates inspecting an HTTP response with the `Response` API.
/// Sends a GET request and examines the response: status code,
/// success/error classification, headers, and `Response.expect`
/// which returns `Err` for non-2xx status codes.
def main(): Unit \ { Http, IO } =
    match Http.get("https://flix.dev/") {
        case Ok(resp) =>
            println("Status:         ${Response.status(resp)}");
            println("Is success?     ${Response.isSuccess(resp)}");
            println("Is client err?  ${Response.isClientError(resp)}");
            println("Is server err?  ${Response.isServerError(resp)}");
            println("Content-Type:   ${Response.contentType(resp)}");
            println("Content-Length: ${Response.contentLength(resp)}");
            println("Server header:  ${Response.headerValue("server", resp)}");
            // expect returns Err for non-2xx responses
            match Response.expect(resp) {
                case Ok(r) =>
                    println("Body length: ${String.length(Response.body(r))}")
                case Err(e) =>
                    println("Unexpected status: ${e}")
            }
        case Err(err) =>
            println("Error: ${err}")
    }
