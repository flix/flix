use Math.Random

def main(): Unit \ IO =
    run {
        let flip = Random.randomFloat64() > 0.5;
        if (flip)
            println("heads")
        else
            println("tails")
    } with Random.runWithIO
