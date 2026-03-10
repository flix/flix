use Sys.{Console}

/// Prompts for a number between 1 and 10, re-prompting on invalid input.
def main(): Unit \ Console =
    let n = Console.readlnWith("Enter a number (1-10): ", s ->
        match Int32.fromString(s) {
            case Some(i) if i >= 1 and i <= 10 => Ok(i)
            case _                              => Err("Please enter a number between 1 and 10.")
        }
    );
    Console.println("You entered: ${n}")
