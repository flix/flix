use Net.Dns
use Net.IpAddr

/// Resolves a hostname to all associated IP addresses using `Dns.lookupAll`.
/// This may return both IPv4 and IPv6 addresses.
def main(): Unit \ { Dns, IO } =
    match Dns.lookupAll("example.com") {
        case Ok(addrs) => List.forEach(a -> println("IP: ${a}"), addrs)
        case Err(err)  => println("Error: ${err}")
    }
