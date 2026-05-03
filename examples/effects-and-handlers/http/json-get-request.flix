use Net.Http
use Net.HttpResponse
use Json.Json
use Json.JsonError
use Json.FromJson

pub enum Uuid({uuid = String})

instance FromJson[Uuid] {
    pub def fromJson(j: Json): Result[JsonError, Uuid] = forM (
        u <- FromJson.decodeAtKey("uuid", j)
    ) yield Uuid.Uuid({uuid = u})
}

/// Sends a GET request to a JSON API and decodes the response into a typed value.
/// Uses `HttpResponse.expectJsonAs` to combine the 2xx status check with
/// parse + decode in a single call. The decoder dispatches via `FromJson[Uuid]`.
def main(): Unit \ { Http, IO } =
    match Http.get("https://httpbin.org/uuid") {
        case Ok(resp) =>
            match HttpResponse.expectJsonAs(resp) {
                case Ok(Uuid.Uuid(x)) => println("UUID: ${x#uuid}")
                case Err(e)           => println("Error: ${e}")
            }
        case Err(err) =>
            println("Error: ${err}")
    }
