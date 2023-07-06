/// Sends the value `x` on the channel `c` after a delay.
def slow(x: Int32, tx: Sender[Int32, r]): Unit \ { r, IO } =
    use Time.Duration.fromSeconds;
    Thread.sleep(fromSeconds(10));
    Channel.send(x, tx);
    ()

/// Reads a value from the channel `c`.
/// Returns the default value `1` if `c` is not ready.
def recvWithDefault(rx: Receiver[Int32, r]): Int32 \ r =
    select {
        case x <- recv(rx) => x
        case _             => 1
    }

/// Reads a value from the channel `c`.
/// Returns the default value `2` if after a timeout.
def recvWithTimeout(r: Region[r], c: Receiver[Int32, r]): Int32 \ { r, IO } =
    use Time.Duration.fromSeconds;
    let t = Channel.timeout(r, fromSeconds(1));
    select {
        case x <- recv(c) => x
        case _ <- recv(t) => 2
    }

/// Creates two channels.
/// Sends values on both after one minute.
/// Receives from both using defaults and timeouts.
def main(): Unit \ IO = region rc {
    let (tx1, rx1) = Channel.buffered(rc, 1);
    let (tx2, rx2) = Channel.buffered(rc, 1);
    spawn slow(123, tx1) @ rc;
    spawn slow(456, tx2) @ rc;
    (recvWithDefault(rx1) + recvWithTimeout(rc, rx2)) |> println
}
