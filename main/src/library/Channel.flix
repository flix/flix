/*
 * Copyright 2022 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod Channel {
    use Time.Duration

    ///
    /// Returns a new buffered channel with capacity for `n` elements.
    ///
    pub def buffered(r: Region[r], n: Int32): (Sender[t, r], Receiver[t, r]) \ Write(r) =
        let m = if (n < 0) 0 else n;
        $CHANNEL_NEW$(r, m)

    ///
    /// Returns a new unbuffered channel (i.e. a channel with zero capacity).
    ///
    pub def unbuffered(r: Region[r]): (Sender[t, r], Receiver[t, r]) \ Write(r) = buffered(r, 0)

    ///
    /// Receives a message from the given channel `r`.
    ///
    /// Blocks until a message is dequeued.
    ///
    pub def recv(receiver: Receiver[t, r]): t \ { Read(r), Write(r) } = $CHANNEL_GET$(receiver)

    ///
    /// Sends the message `m` on the given channel `s`.
    ///
    /// Blocks until the message is enqueued.
    ///
    pub def send(m: t, sender: Sender[t, r]): Unit \ Write(r) with Sendable[t] = $CHANNEL_PUT$(sender, m)

    ///
    /// Sends the message `m` on the given channel `s`.
    ///
    /// Blocks until the message is enqueued.
    ///
    /// Identical to `send` but doesn't require `Sendable`.
    /// It is up to programmer to ensure that race conditions cannot arise due to concurrent access to `m`.
    ///
    @Unsafe
    pub def unsafeSend(m: t, sender: Sender[t, r]): Unit \ Write(r) = $CHANNEL_PUT$(sender, m)

    ///
    /// Returns a channel that receives the `Unit` message after duration `d`.
    ///
    pub def timeout(r: Region[r], d: Duration): Receiver[Unit, r] \ { Read(r), Write(r), IO } =
        let (tx, rx) = Channel.unbuffered(r);
        spawn { Thread.sleep(d); send((), tx) } @ r;
        rx

}
