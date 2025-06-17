def main(): Unit \ {NonDet, IO} =
    run {
        let flip = Random.randomBool();
        if (flip)
            println("heads")
        else
            println("tails")
    } with Random.runWithIO
