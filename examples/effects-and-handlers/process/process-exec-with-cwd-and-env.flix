use Sys.Process

/// Demonstrates `Process.execWithCwd` and `Process.execWithEnv`.
/// `execWithCwd` sets the working directory for the spawned process.
/// `execWithEnv` passes additional environment variables to it.
def main(): Unit \ { Process, IO } =
    match Process.execWithCwd("java", "-version" :: Nil, Some("/tmp")) {
        case Result.Ok(_)    => println("execWithCwd succeeded.")
        case Result.Err(err) => println("execWithCwd failed: ${err}")
    };
    match Process.execWithEnv("java", "-version" :: Nil, Map#{"MY_VAR" => "hello"}) {
        case Result.Ok(_)    => println("execWithEnv succeeded.")
        case Result.Err(err) => println("execWithEnv failed: ${err}")
    }
