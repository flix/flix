/*
 * Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Channel {

    use Time.TimeUnit

    import java.lang.{Thread => JThread}

    ///
    /// Returns a new buffered channel with capacity for `n` elements.
    ///
    pub def buffered(n: Int32): (Sender[t], Receiver[t]) \ Chan =
        let m = if (n < 0) 0 else n;
        %%CHANNEL_NEW%%(m)

    ///
    /// Returns a new unbuffered channel (i.e. a channel with zero capacity).
    ///
    pub def unbuffered(): (Sender[t], Receiver[t]) \ Chan = buffered(0)

    ///
    /// Receives a message from the given channel `r`.
    ///
    /// Blocks until a message is dequeued.
    ///
    pub def recv(receiver: Receiver[t]): t \ { Chan, NonDet } = %%CHANNEL_GET%%(receiver)

    ///
    /// Sends the message `m` on the given channel `s`.
    ///
    /// Blocks until the message is enqueued.
    ///
    pub def send(m: t, sender: Sender[t]): Unit \ Chan = %%CHANNEL_PUT%%(sender, m)

    ///
    /// Returns a channel that receives the `Unit` message after `duration` in the given time `unit`.
    ///
    pub def timeout(duration: Int32, unit: TimeUnit): Receiver[Unit] \ { Chan, IO } =
        let millis = Int32.toInt64(duration * TimeUnit.toMilliseconds(unit));
        let (tx, rx) = Channel.buffered(1);
        spawn { JThread.sleep(millis); send((), tx) } @ Static;
        rx

}
