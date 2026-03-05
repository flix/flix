use Net.Http
use Net.Request
use Net.Response

def main(): Unit \ IO =
    run {
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
    } with Http.runWithIO
