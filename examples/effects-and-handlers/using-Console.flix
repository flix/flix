def main(): Unit \ IO =
    run {
        Console.println("Please enter your name: ");
        let name = Console.readln();
        Console.println("Hello ${name}")
    } with Console.runWithIO
