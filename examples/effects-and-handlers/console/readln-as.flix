use Sys.Console

/// Prompts for an integer, re-prompting until the input parses, and prints its square.
def main(): Unit \ Console =
    let n: Option[Int32] = Console.readlnAs("Enter an integer: ");
    match n {
        case Some(i) => Console.println("${i} squared is ${i * i}.")
        case None    => Console.println("No integer given.")
    }
