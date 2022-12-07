 /// Mooo's `n` times on channel `c`.
def mooo(tx: Sender[String, r], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => Channel.send("Mooo!", tx); mooo(tx, x - 1)
    }

/// Meow's `n` times on channel `c`.
def meow(tx: Sender[String, r], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => Channel.send("Meow!", tx); meow(tx, x - 1)
    }

/// Hiss'es `n` times on channel `c`.
def hiss(tx: Sender[String, r], n: Int32): Unit \ IO =
    match n {
        case 0 => ()
        case x => Channel.send("Hiss!", tx); hiss(tx, x - 1)
    }

/// Start the animal farm...
def main(): Unit \ IO = region r {
    let (tx1, rx1) = Channel.buffered(r, 10);
    let (tx2, rx2) = Channel.buffered(r, 10);
    let (tx3, rx3) = Channel.buffered(r, 10);
    spawn mooo(tx1, 0) @ r;
    spawn meow(tx2, 3) @ r;
    spawn hiss(tx3, 7) @ r;
    select {
        case m <- recv(rx1) => m |> println
        case m <- recv(rx2) => m |> println
        case m <- recv(rx3) => m |> println
    }
}
