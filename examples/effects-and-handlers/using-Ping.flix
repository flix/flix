use Net.Dns
use Net.Ping
use Time.TimeUnit

/// Resolves a hostname with `Dns.lookup` and checks reachability
/// with `Ping.isReachable` using a 5-second timeout.
def main(): Unit \ { Dns, Ping, IO } =
    match Dns.lookup("example.com") {
        case Ok(addr) =>
            match Ping.isReachable(addr, 5, TimeUnit.Seconds) {
                case Ok(true)  => println("Host is reachable")
                case Ok(false) => println("Host is not reachable")
                case Err(err)  => println("Error: ${err}")
            }
        case Err(err) => println("DNS error: ${err}")
    }
