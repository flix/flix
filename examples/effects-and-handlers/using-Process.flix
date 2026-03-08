def main(): Unit \ IO =
    run {
        match Process.exec("ls", Nil) {
            case Result.Ok(_)    => ()
            case Result.Err(err) => println("Unable to execute process: ${err}")
        }
    } with Process.runWithIO
