def main(): Unit \ IO =
    run {
        let data = List#{"Hello", "World"};
        match Fs.WriteLines.writeLines(lines = data, "data.txt") {
            case Result.Ok(_)    => ()
            case Result.Err(err) =>
                println("Unable to write file. Error: ${err}")
        }
    } with Fs.WriteLines.runWithIO
