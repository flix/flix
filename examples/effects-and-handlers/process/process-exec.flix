use Sys.Process

/// Executes a command using `Process.exec` and pattern-matches on the result.
/// The `Process` effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Process, IO } =
    match Process.exec("java", "-version" :: Nil) {
        case Result.Ok(_)    => println("Process started successfully.")
        case Result.Err(err) => println("Unable to execute process: ${err}")
    }
