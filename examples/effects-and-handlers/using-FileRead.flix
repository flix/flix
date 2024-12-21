def main(): Unit \ {Sys, Net, IO} =
    let result = App.runAllWithResult(() ->
        let lines = FileRead.readLines("Main.flix");
        List.forEach(println, lines)
    );
    println(result)
