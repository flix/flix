
// The main entry point.
pub def main(): Result[IoError, Unit] \ IO + Net + Sys =
    let ip = IpAddr.fromBytes(127i8 :: 0i8 :: 0i8 :: 1i8 :: Nil);
    let port = 1234i16;
    run {
        Server.launch(ip, port)
    } with TcpBindWithResult.runWithIO
    with TcpAcceptWithResult.runWithIO
    with Logger.runWithIO
