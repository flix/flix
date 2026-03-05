use Net.Http
use Net.Response

def main(): Unit \ IO =
    run {
        match Http.get("http://example.com/") {
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
    } with Http.runWithIO
