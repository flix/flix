 /// Mooo's `n` times on channel `c`.
def mooo(c: Channel[Str], n: Int): Unit = match n with {
    case 0 => ()
    case n => c <- "Mooo!"; mooo(c, n - 1)
}

/// Meow's `n` times on channel `c`.
def meow(c: Channel[Str], n: Int): Unit = match n with {
    case 0 => ()
    case n => c <- "Meow!"; meow(c, n - 1)
}

/// Hiss'es `n` times on channel `c`.
def hiss(c: Channel[Str], n: Int): Unit = match n with {
    case 0 => ()
    case n => c <- "Hiss!"; hiss(c, n - 1)
}

/// Start the animal farm...
def main(): Str =
    let c1 = chan Str 1;
    let c2 = chan Str 1;
    let c3 = chan Str 1;
    spawn mooo(c1, 0);
    spawn meow(c2, 3);
    spawn hiss(c3, 7);
    select {
        case mooo <- c1 => mooo
        case meow <- c2 => meow
        case hiss <- c3 => hiss
    }
