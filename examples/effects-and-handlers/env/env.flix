use Sys.Env

/// Reads the user's home directory, current working directory, and OS name.
/// The Env effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ { Env, IO } =
    let home = Env.getUserHomeDirectory();
    println("Home: ${home}");
    let cwd = Env.getCurrentWorkingDirectory();
    println("CWD: ${cwd}");
    let os = Env.getOsName();
    println("OS: ${os}")
