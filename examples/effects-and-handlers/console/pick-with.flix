use Sys.Console

/// Presents a numbered menu and re-prompts until the user makes a valid selection.
def main(): Unit \ Console =
    let color = Console.pickWith("Pick a color:", "Red" :: "Green" :: "Blue" :: Nil);
    Console.println("You picked: ${color}")
