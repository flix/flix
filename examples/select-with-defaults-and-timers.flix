/// Sends the value `x` on the channel `c` after a delay.
def slow(x: Int32, c: Channel[Int32]): Unit & Impure =
    import java.lang.Thread:sleep(Int64);
    sleep(Duration.oneMinute() / 1_000_000i64);
    c <- x;
    ()

/// Reads a value from the channel `c`.
/// Returns the default value `1` if `c` is not ready.
def recvWithDefault(c: Channel[Int32]): Int32 & Impure =
    select {
        case x <- c => x
        case _      => 1
    }

/// Reads a value from the channel `c`.
/// Returns the default value `2` if after a timeout.
def recvWithTimeout(c: Channel[Int32]): Int32 & Impure =
    select {
        case x <- c                   => x
        case _ <- Timer.seconds(1i64) => 2
    }

/// Creates two channels `c1` and `c2`.
/// Sends values on both after one minute.
/// Receives from both using defaults and timeouts.
def main(_args: Array[String]): Int32 & Impure = {
  let c1 = chan Int32 1;
  let c2 = chan Int32 1;
  spawn slow(123, c1);
  spawn slow(456, c2);
  (recvWithDefault(c1) + recvWithTimeout(c2)) |> println;
  0 // exit code
}
