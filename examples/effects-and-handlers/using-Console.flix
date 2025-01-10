def main(): Unit \ {Sys, IO} =
    run {
        let m = "${Chalk.blue("Hello")} ${Chalk.green("World!")}!";
        Console.println(m)
    } with Console.runWithIO
      with Environment.runWithIO
