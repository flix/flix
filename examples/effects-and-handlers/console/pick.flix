use Sys.Console

/// Presents a numbered menu and prints the user's selection.
def main(): Unit \ Console =
    let color = Console.pick("Pick a color:", "Red" :: "Green" :: "Blue" :: Nil);
    match color {
        case Some(c) => Console.println("You picked: ${c}")
        case None    => Console.println("Invalid selection.")
    }
