use Net.Http
use Net.Request
use Net.Response

/// Sends a POST request with a JSON body and prints the response status and body.
/// Uses `Request.post` to create the request and sets content type and accept headers to JSON.
def main(): Unit \ { Http, IO } =
    let body = "{\"name\": \"Alice\", \"age\": 30}";
    let req = Request.post("https://flix.dev/api/users", body)
                |> Request.withContentType("application/json")
                |> Request.withAccept("application/json");
    match Http.send(req) {
        case Ok(resp) =>
            println("Status: ${Response.status(resp)}");
            println("Body: ${Response.body(resp)}")
        case Err(err) =>
            println("Error: ${err}")
    }
