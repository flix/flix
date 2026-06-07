use Net.Http
use Net.HttpRequest
use Net.HttpResponse
use Util.Json.Json
use Util.Json.Json.{JNumber, JString}
use Util.Json.toCompactString
use Util.Json.ToJson

/// Sends a POST request with a typed JSON body and prints the response status and body.
/// Serialises the body via the `ToJson` trait (`toCompactString` of `ToJson.toJson`)
/// and sets the `Content-Type` header to `application/json`.
/// Here the body is a `Map[String, Json]`; any value with a `ToJson` instance works.
def main(): Unit \ { Http, IO } =
    let body: Map[String, Json] = Map#{
        "name" => JString("Asterix"),
        "age"  => JNumber(35.0ff)
    };
    let req = HttpRequest.post("https://flix.dev/api/users", toCompactString(ToJson.toJson(body)))
                |> HttpRequest.withContentType("application/json")
                |> HttpRequest.withAccept("application/json");
    match Http.send(req) {
        case Ok(resp) =>
            println("Status: ${HttpResponse.status(resp)}");
            println("Body: ${HttpResponse.body(resp)}")
        case Err(err) =>
            println("Error: ${err}")
    }
