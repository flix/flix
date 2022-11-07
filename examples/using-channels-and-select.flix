 /// Mooo's `n` times on channel `c`.
def mooo(s: Sender[String], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => Channel.send("Mooo!", s); mooo(s, x - 1)
    }

/// Meow's `n` times on channel `c`.
def meow(s: Sender[String], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => Channel.send("Meow!", s); meow(s, x - 1)
    }

/// Hiss'es `n` times on channel `c`.
def hiss(s: Sender[String], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => Channel.send("Hiss!", s); hiss(s, x - 1)
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
        case m <- recv(r1) => m |> println
        case m <- recv(r2) => m |> println
        case m <- recv(r3) => m |> println
    }
}
