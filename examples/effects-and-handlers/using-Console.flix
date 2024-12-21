def main(): Unit \ {Net, Sys, IO} =
    App.runAll(() -> {
        let m = "${Chalk.blue("Hello")} ${Chalk.green("World!")}!";
        Console.println(m)
    })
