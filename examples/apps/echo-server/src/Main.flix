
// The main entry point.
pub def main(): Unit \ IO + Net + Sys =
    let ip = IpAddr.fromBytes(127i8 :: 0i8 :: 0i8 :: 1i8 :: Nil);
    let port = 1234i16;
    run {
        match Server.launch(ip, port) {
            case Ok(_) => ()
            case Err(err) => Logger.fatal(err)
        }
    } with TcpBindWithResult.runWithIO
    with TcpAcceptWithResult.runWithIO
    with Logger.runWithIO
