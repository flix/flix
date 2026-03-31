use Net.Http
use Net.HttpRequest
use Net.HttpResponse

/// Sends a POST request with a JSON body and prints the response status and body.
/// Uses `HttpRequest.post` to create the request and sets content type and accept headers to JSON.
def main(): Unit \ { Http, IO } =
    let body = "{\"name\": \"Asterix\", \"age\": 35}";
    let req = HttpRequest.post("https://flix.dev/api/users", body)
                |> HttpRequest.withContentType("application/json")
                |> HttpRequest.withAccept("application/json");
    match Http.send(req) {
        case Ok(resp) =>
            println("Status: ${HttpResponse.status(resp)}");
            println("Body: ${HttpResponse.body(resp)}")
        case Err(err) =>
            println("Error: ${err}")
    }
