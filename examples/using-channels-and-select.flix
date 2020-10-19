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
def main(): String & Impure =
    let c1 = chan String 1;
    let c2 = chan String 1;
    let c3 = chan String 1;
    spawn mooo(c1, 0);
    spawn meow(c2, 3);
    spawn hiss(c3, 7);
    select {
        case mooo <- c1 => mooo
        case meow <- c2 => meow
        case hiss <- c3 => hiss
    }
