use Net.Http
use Net.HttpResponse

/// Demonstrates inspecting an HTTP response with the `HttpResponse` API.
/// Sends a GET request and examines the response: status code,
/// success/error classification, headers, and `HttpResponse.expect`
/// which returns `Err` for non-2xx status codes.
def main(): Unit \ { Http, IO } =
    match Http.get("https://flix.dev/") {
        case Ok(resp) =>
            println("Status:         ${HttpResponse.status(resp)}");
            println("Is success?     ${HttpResponse.isSuccess(resp)}");
            println("Is client err?  ${HttpResponse.isClientError(resp)}");
            println("Is server err?  ${HttpResponse.isServerError(resp)}");
            println("Content-Type:   ${HttpResponse.contentType(resp)}");
            println("Content-Length: ${HttpResponse.contentLength(resp)}");
            println("Server header:  ${HttpResponse.headerValue("server", resp)}");
            // expect returns Err for non-2xx responses
            match HttpResponse.expect(resp) {
                case Ok(r) =>
                    println("Body length: ${String.length(HttpResponse.body(r))}")
                case Err(e) =>
                    println("Unexpected status: ${e}")
            }
        case Err(err) =>
            println("Error: ${err}")
    }
