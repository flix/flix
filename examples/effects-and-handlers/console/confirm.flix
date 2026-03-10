use Sys.Console

/// Asks the user for confirmation before proceeding.
/// Empty input defaults to `true` (the `[Y/n]` hint).
def main(): Unit \ Console =
    let proceed = Console.confirm("Deploy to production?", default = true);
    if (proceed)
        Console.println("Deploying...")
    else
        Console.println("Aborted.")
