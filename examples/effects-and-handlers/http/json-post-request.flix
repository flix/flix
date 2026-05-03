use Net.Http
use Net.HttpRequest
use Net.HttpResponse
use Json.Json
use Json.Json.{JNumber, JString}

/// Sends a POST request with a typed JSON body and prints the response status and body.
/// Uses `HttpRequest.withJson` to serialise via the `ToJson` trait, which also
/// sets the `Content-Type` header to `application/json` in one call.
/// Here the body is a `Map[String, Json]`; any value with a `ToJson` instance works.
def main(): Unit \ { Http, IO } =
    let body: Map[String, Json] = Map#{
        "name" => JString("Asterix"),
        "age"  => JNumber(35.0ff)
    };
    let req = HttpRequest.post("https://flix.dev/api/users", "")
                |> HttpRequest.withJson(body)
                |> HttpRequest.withAccept("application/json");
    match Http.send(req) {
        case Ok(resp) =>
            println("Status: ${HttpResponse.status(resp)}");
            println("Body: ${HttpResponse.body(resp)}")
        case Err(err) =>
            println("Error: ${err}")
    }
