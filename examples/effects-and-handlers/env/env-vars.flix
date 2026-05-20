use Sys.Env

/// Reads a specific environment variable with `getVar` and retrieves
/// all environment variables as a map with `getEnv`.
def main(): Unit \ { Env, IO } =
    let path = Env.getVar("PATH");
    println("PATH: ${path}");
    let all = Env.getEnv();
    println("Total env vars: ${Map.size(all)}")
