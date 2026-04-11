///
/// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
///
/// Exercises:
/// - buffered + unbuffered channels
/// - blocking semantics (full buffer, rendezvous)
/// - passing heap values through channels (String)
/// - GC visibility for queued payloads (native GC stress)
///
def assertTrue(cond: Bool, msg: String): Unit =
    if (cond) () else bug!(msg)

def allocateGarbage(n: Int32): Unit = {
    def loop(i: Int32): Unit =
        if (i >= n) ()
        else {
            // Allocate enough heap junk to drive GC under stress settings.
            let s = "g" + Int32.toString(i);
            let _ = s + s + s;
            loop(i + 1i32)
        };
    loop(0i32)
}

def main(): Unit \ {Chan, NonDet, IO} = {
    // -----------------------------------------------------------------------
    // Buffered channel (cap=1): "buffer full" blocks the second sender.
    // -----------------------------------------------------------------------
    let (tx, rx) = Channel.buffered(1);

    // First message: must remain live in the channel buffer while we allocate garbage.
    Channel.send("one", tx);

    // Second message: blocks until we receive the first.
    spawn {
        Channel.send("two", tx);
        ()
    } @ Static;

    let _ = allocateGarbage(20_000);

    let a = Channel.recv(rx);
    let b = Channel.recv(rx);

    let _ = assertTrue(a == "one", "expected first message 'one', got: " + a);
    let _ = assertTrue(b == "two", "expected second message 'two', got: " + b);

    // -----------------------------------------------------------------------
    // Unbuffered channel: rendezvous between sender and receiver.
    // -----------------------------------------------------------------------
    let (tx2, rx2) = Channel.unbuffered();

    spawn {
        Channel.send("ping", tx2);
        ()
    } @ Static;

    let c = Channel.recv(rx2);
    let _ = assertTrue(c == "ping", "expected rendezvous message 'ping', got: " + c);

    println("channel-rendezvous: ok");
    ()
}
