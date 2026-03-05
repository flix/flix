use Net.Https
use Net.Response

def main(): Unit \ IO =
    run {
        // The Https handler validates that URLs use the https:// scheme.
        match Https.get("https://example.com/") {
            case Ok(resp) =>
                println("Status: ${Response.status(resp)}");
                println("Body: ${String.takeLeft(80, Response.body(resp))}...")
            case Err(err) =>
                println("Error: ${err}")
        }
    } with Https.runWithIO
