use Sys.Console

/// Prints a prompt, reads the user's name, and greets them.
/// The Console effect has a default handler, so no explicit `runWithIO` is needed.
def main(): Unit \ Console =
    Console.print("What is your name? ");
    let name = Console.readln();
    Console.println("Hello ${name}!")
