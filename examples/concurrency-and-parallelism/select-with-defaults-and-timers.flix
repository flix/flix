/// Sends the value `x` on the channel `c` after a delay.
def slow(x: Int32, tx: Sender[Int32]): Unit \ { Global, IO } =
    use Time.Duration.fromSeconds;
    Thread.sleep(fromSeconds(10));
    Channel.send(x, tx);
    ()

/// Reads a value from the channel `c`.
/// Returns the default value `1` if `c` is not ready.
def recvWithDefault(rx: Receiver[Int32]): Int32 \ { Global, NonDet } =
    select {
        case x <- recv(rx) => x
        case _             => 1
    }

/// Reads a value from the channel `c`.
/// Returns the default value `2` if after a timeout.
def recvWithTimeout(c: Receiver[Int32]): Int32 \ { Global, NonDet, IO } =
    use Time.Duration.fromSeconds;
    let t = Channel.timeout(fromSeconds(1));
    select {
        case x <- recv(c) => x
        case _ <- recv(t) => 2
    }

/// Creates two channels.
/// Sends values on both after one minute.
/// Receives from both using defaults and timeouts.
def main(): Unit \ { Global, NonDet, IO } = region rc {
    let (tx1, rx1) = Channel.buffered(1);
    let (tx2, rx2) = Channel.buffered(1);
    spawn slow(123, tx1) @ rc;
    spawn slow(456, tx2) @ rc;
    (recvWithDefault(rx1) + recvWithTimeout(rx2)) |> println
}
