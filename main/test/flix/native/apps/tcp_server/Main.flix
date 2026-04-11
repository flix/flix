///
/// A small "real program" fixture for the LLVM-native backend.
///
/// Exercises:
/// - TCP bind/accept + read/write
/// - ephemeral ports (bind to port 0, then query `TcpServer.localPort`)
///
/// Protocol:
/// - read a single byte: 65 ('A')
/// - write a single byte: 66 ('B')
///
def expectOk(res: Result[a, b]): b with ToString[a] = match res {
    case Ok(v) => v
    case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
}

def main(): Unit \ IO =
    run {
        let ip = match IpAddr.fromString("127.0.0.1") {
            case Some(i) => i
            case None => bug!("bad IpAddr literal")
        };

        let server = TcpBindWithResult.bind(ip, 0i32) |> expectOk;
        let port = TcpServer.localPort(server) |> expectOk;
        println("PORT=" + Int32.toString(port));

        let socket = TcpAcceptWithResult.accept(server) |> expectOk;

        region rc {
            let buf: Array[Int8, rc] = Array.empty(rc, 1);
            let n = TcpSocket.read(buf, socket) |> expectOk;
            let _ =
                if (n != 1i32)
                    bug!("tcp-server: expected 1 byte, got " + Int32.toString(n))
                else ();

            let b0 = Array.get(0, buf);
            let _ =
                if (b0 != 65i8)
                    bug!("tcp-server: expected 65, got " + Int8.toString(b0))
                else ();

            let pong: Array[Int8, rc] = Array#{66i8} @ rc;
            let _ = TcpSocket.write(pong, socket) |> expectOk;
            ()
        };

        let _ = TcpSocket.close(socket) |> expectOk;
        let _ = TcpServer.close(server) |> expectOk;

        println("tcp-server: ok");
        ()
    } with TcpBindWithResult.runWithIO
      with TcpAcceptWithResult.runWithIO

