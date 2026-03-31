use Net.Dns
use Net.TcpConnect

/// Connects to tcpbin.com's TCP echo server on port 4242,
/// sends a message, and reads back the echoed response.
/// The TcpConnect and Dns effects have default handlers,
/// so no explicit `runWithIO` is needed.
def main(): Unit \ { Dns, TcpConnect, IO } = region rc {
    match Dns.lookup("tcpbin.com") {
        case Err(err) => println("DNS error: ${err}")
        case Ok(ip)   =>
            match TcpConnect.connect(ip, 4242i16) {
                case Err(err) => println("Connect error: ${err}")
                case Ok(socket) =>
                    // Send a message to the echo server (newline triggers the echo)
                    let msg = "Hello, tcpbin!\n";
                    let sendBuf = Vector.toArray(rc, String.toBytes(msg));
                    discard Writable.write(sendBuf, socket);
                    // Read the echoed response
                    let recvBuf = Array.repeat(rc, 1024, (0i8: Int8));
                    match Readable.read(recvBuf, socket) {
                        case Err(err) => println("Read error: ${err}")
                        case Ok(n)    =>
                            let bytes = Array.slice(rc, start = 0, end = n, recvBuf);
                            let reply = String.fromBytes(Array.toVector(bytes));
                            println("Echo: ${reply}")
                    }
            }
    }
}
