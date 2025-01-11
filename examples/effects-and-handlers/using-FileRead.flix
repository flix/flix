def main(): Unit \ IO =
    run {
        match FileReadWithResult.readLines("Main.flix") {
            case Result.Ok(lines) => List.forEach(println, lines)
            case Result.Err(e)    => println(e)
        }
    } with FileReadWithResult.runWithIO
