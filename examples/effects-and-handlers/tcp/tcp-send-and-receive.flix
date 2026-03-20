use Net.IpAddr
use Net.Ipv4Addr
use Net.TcpAccept
use Net.TcpBind
use Net.TcpConnect

/// Binds a TCP server on localhost, connects to it from the same
/// thread (the OS queues the connection in the listen backlog),
/// accepts the connection, and exchanges a message.
/// Uses all three TCP effects: TcpBind, TcpConnect, and TcpAccept.
def main(): Unit \ { TcpBind, TcpConnect, TcpAccept, IO } = region rc {
    let localhost = IpAddr.V4(Ipv4Addr.localhost());
    let port = 9876i16;
    match TcpBind.bind(localhost, port) {
        case Err(err) => println("Bind error: ${err}")
        case Ok(server) =>
            println("Server listening on 127.0.0.1:9876");
            match TcpConnect.connect(localhost, port) {
                case Err(err) => println("Connect error: ${err}")
                case Ok(client) =>
                    match TcpAccept.accept(server) {
                        case Err(err) => println("Accept error: ${err}")
                        case Ok(serverSide) =>
                            // Client sends a message
                            let msg = "Hello from client!";
                            let sendBuf = Vector.toArray(rc, String.toBytes(msg));
                            discard Writable.write(sendBuf, client);
                            // Server reads the message
                            let recvBuf = Array.repeat(rc, 1024, (0i8: Int8));
                            match Readable.read(recvBuf, serverSide) {
                                case Err(err) => println("Read error: ${err}")
                                case Ok(n)    =>
                                    let bytes = Array.slice(rc, start = 0, end = n, recvBuf);
                                    let received = String.fromBytes(Array.toVector(bytes));
                                    println("Server received: ${received}")
                            }
                    }
            }
    }
}
