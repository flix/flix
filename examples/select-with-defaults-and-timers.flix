/// Sends the value `x` on the channel `c` after a delay.
def slow(x: Int32, s: Sender[Int32, Static]): Unit \ IO =
    use Time/Duration.fromSeconds;
    Thread.sleep(fromSeconds(10));
    Channel.send(x, s);
    ()

/// Reads a value from the channel `c`.
/// Returns the default value `1` if `c` is not ready.
def recvWithDefault(r: Receiver[Int32, Static]): Int32 \ IO =
    select {
        case x <- recv(r) => x
        case _            => 1
    }

/// Reads a value from the channel `c`.
/// Returns the default value `2` if after a timeout.
def recvWithTimeout(c: Receiver[Int32, Static]): Int32 \ IO =
    use Time/Duration.fromSeconds;
    let t = Channel.timeout(fromSeconds(1));
    select {
        case x <- recv(c) => x
        case _ <- recv(t) => 2
    }

/// Creates two channels `c1` and `c2`.
/// Sends values on both after one minute.
/// Receives from both using defaults and timeouts.
def main(): Unit \ IO = {
    let (s1, r1) = Channel.buffered(Static, 1);
    let (s2, r2) = Channel.buffered(Static, 1);
    spawn slow(123, s1);
    spawn slow(456, s2);
    (recvWithDefault(r1) + recvWithTimeout(r2)) |> println
}
