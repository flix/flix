use Net.Http
use Net.Response

def main(): Unit \ IO =
    run {
        match Http.get("http://example.com/") {
            case Ok(resp) =>
                println("Status: ${Response.status(resp)}");
                println("OK? ${Response.isOk(resp)}");
                let preview = String.takeLeft(80, Response.body(resp));
                println("Body: ${preview}...")
            case Err(err) =>
                println("Error: ${err}")
        }
    } with Http.runWithIO
