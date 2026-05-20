use Net.Dns
use Net.IpAddr

/// Resolves a hostname to a single IP address using `Dns.lookup`.
/// The Dns effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Dns, IO } =
    match Dns.lookup("example.com") {
        case Ok(addr) => println("IP: ${addr}")
        case Err(err) => println("Error: ${err}")
    }
