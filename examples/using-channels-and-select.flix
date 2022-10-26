 /// Mooo's `n` times on channel `c`.
def mooo(c: Sender[String], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => c <- "Mooo!"; mooo(c, x - 1)
    }

/// Meow's `n` times on channel `c`.
def meow(s: Sender[String], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => s <- "Meow!"; meow(s, x - 1)
    }

/// Hiss'es `n` times on channel `c`.
def hiss(s: Sender[String], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => s <- "Hiss!"; hiss(s, x - 1)
    }

/// Start the animal farm...
def main(): Unit \ IO = {
    let (s1, r1) = Channel.buffered(10);
    let (s2, r2) = Channel.buffered(10);
    let (s3, r3) = Channel.buffered(10);
    spawn mooo(s1, 0);
    spawn meow(s2, 3);
    spawn hiss(s3, 7);
    select {
        case m <- r1 => m |> println
        case m <- r2 => m |> println
        case m <- r3 => m |> println
    }
}
