 /// Mooo's `n` times on channel `c`.
def mooo(c: Channel[String], n: Int): Unit & Impure =
    match n {
        case 0 => ()
        case x => c <- "Mooo!"; mooo(c, x - 1)
    }

/// Meow's `n` times on channel `c`.
def meow(c: Channel[String], n: Int): Unit & Impure =
    match n {
        case 0 => ()
        case x => c <- "Meow!"; meow(c, x - 1)
    }

/// Hiss'es `n` times on channel `c`.
def hiss(c: Channel[String], n: Int): Unit & Impure =
    match n {
        case 0 => ()
        case x => c <- "Hiss!"; hiss(c, x - 1)
    }

/// Start the animal farm...
def main(_args: Array[String]): String & Impure = {
    let c1 = chan String 1;
    let c2 = chan String 1;
    let c3 = chan String 1;
    spawn mooo(c1, 0);
    spawn meow(c2, 3);
    spawn hiss(c3, 7);
    select {
        case m <- c1 => m |> println
        case m <- c2 => m |> println
        case m <- c3 => m |> println
    };
    0 // exit code
}
