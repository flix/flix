def main(): Unit \ {Exec, IO} =
    run {
        match ProcessWithResult.exec("ls", Nil) {
            case Result.Ok(_)    => ()
            case Result.Err(err) => println("Unable to execute process: ${err}")
        }
    } with ProcessWithResult.runWithIO
