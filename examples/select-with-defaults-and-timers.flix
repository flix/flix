/// Sends the value `x` on the channel `c` after a delay.
def slow(x: Int32, tx: Sender[Int32, r]): Unit \ { Write(r), IO } =
    use Time.Duration.fromSeconds;
    Thread.sleep(fromSeconds(10));
    Channel.send(x, tx);
    ()

/// Reads a value from the channel `c`.
/// Returns the default value `1` if `c` is not ready.
def recvWithDefault(rx: Receiver[Int32, r]): Int32 \ { Read(r), Write(r) } =
    select {
        case x <- recv(rx) => x
        case _             => 1
    }

/// Reads a value from the channel `c`.
/// Returns the default value `2` if after a timeout.
def recvWithTimeout(r: Region[r], c: Receiver[Int32, r]): Int32 \ { Read(r), Write(r), IO } =
    use Time.Duration.fromSeconds;
    let t = Channel.timeout(r, fromSeconds(1));
    select {
        case x <- recv(c) => x
        case _ <- recv(t) => 2
    }

/// Creates two channels.
/// Sends values on both after one minute.
/// Receives from both using defaults and timeouts.
def main(): Unit \ IO = region r {
    let (tx1, rx1) = Channel.buffered(r, 1);
    let (tx2, rx2) = Channel.buffered(r, 1);
    spawn slow(123, tx1) @ r;
    spawn slow(456, tx2) @ r;
    (recvWithDefault(rx1) + recvWithTimeout(r, rx2)) |> println
}
