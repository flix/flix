use Fs.WriteLines

def main(): Unit \ IO =
    run {
        let data = List#{"Hello", "World"};
        match WriteLines.writeLines(lines = data, "data.txt") {
            case Result.Ok(_)    => ()
            case Result.Err(err) =>
                println("Unable to write file. Error: ${err}")
        }
    } with WriteLines.runWithIO
