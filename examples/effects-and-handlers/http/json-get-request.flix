use Net.Http
use Net.HttpResponse
use Util.Json
use Util.Json.FromJson
use Util.Json.JsonError

pub enum Uuid({uuid = String})

instance FromJson[Uuid] {
    pub def fromJson(j: Json): Result[JsonError, Uuid] = forM (
        u <- FromJson.decodeAtKey("uuid", j)
    ) yield Uuid.Uuid({uuid = u})
}

/// Sends a GET request to a JSON API and decodes the response into a typed value.
/// Composes the 2xx status check (`HttpResponse.expect`) with JSON parse + decode
/// (`Json.decode` over `HttpResponse.body`) — each step keeps its own typed
/// error so the caller can distinguish HTTP failures from JSON failures. The
/// decoder dispatches via `FromJson[Uuid]`.
def main(): Unit \ { Http, IO } =
    match Http.get("https://httpbin.org/uuid") {
        case Ok(resp) => match HttpResponse.expect(resp) {
            case Ok(okResp) => match Json.decode(HttpResponse.body(okResp)) {
                case Ok(Uuid.Uuid(x)) => println("UUID: ${x#uuid}")
                case Err(err)         => println("Decode error: ${err}")
            }
            case Err(err) => println("HTTP error: ${err}")
        }
        case Err(err) => println("Error: ${err}")
    }
