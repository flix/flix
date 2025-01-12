def main(): Unit \ {NonDet, IO} =
    run {
        Console.println("Please enter your name:");
        let name = Console.readln();
        let flip = Random.randomBool();
        if (flip)
            Console.println("Pleased to meet you, ${name}")
        else
            Console.println("Oh no, not you, ${name}")
    } with Console.runWithIO
      with Random.runWithIO
