///
/// A small "real program" fixture for the LLVM-native backend.
///
/// Exercises:
/// - TCP connect + read/write
/// - argv parsing (`Env.getArgs`)
///
def expectOk(res: Result[a, b]): b with ToString[a] = match res {
    case Ok(v) => v
    case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
}

def main(): Unit \ IO =
    run {
        let args = Env.getArgs();
        let (hostStr, portStr) = match args {
            case h :: p :: Nil => (h, p)
            case _ => bug!("expected args: <host> <port>")
        };

        let ip = match IpAddr.fromString(hostStr) {
            case Some(i) => i
            case None => bug!("bad IpAddr: " + hostStr)
        };
        let port = match Int32.fromString(portStr) {
            case Some(p) => p
            case None => bug!("bad Int32: " + portStr)
        };

        let socket = TcpConnectWithResult.connect(ip, port) |> expectOk;

        region rc {
            let ping: Array[Int8, rc] = Array#{65i8} @ rc;
            let _ = TcpSocket.write(ping, socket) |> expectOk;

            let buf: Array[Int8, rc] = Array.empty(rc, 1);
            let n = TcpSocket.read(buf, socket) |> expectOk;
            let _ =
                if (n != 1i32)
                    bug!("tcp-client: expected 1 byte, got " + Int32.toString(n))
                else ();

            let b0 = Array.get(0, buf);
            let _ =
                if (b0 != 66i8)
                    bug!("tcp-client: expected 66, got " + Int8.toString(b0))
                else ();

            ()
        };

        let _ = TcpSocket.close(socket) |> expectOk;
        println("tcp-client: ok");
        ()
    } with Env.runWithIO
      with TcpConnectWithResult.runWithIO

