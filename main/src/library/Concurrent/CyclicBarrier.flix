/*
 * Copyright 2022 Paul Butcher
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Concurrent.CyclicBarrier {
    import java.util.concurrent.{CyclicBarrier => JCyclicBarrier}

    ///
    /// A wrapper around a Java CyclicBarrier.
    ///
    pub enum CyclicBarrier(JCyclicBarrier)

    ///
    /// Create a new CyclicBarrier which will wait for `parties` threads
    ///
    pub def newCyclicBarrier(parties: Int32): CyclicBarrier \ IO =
        CyclicBarrier(new JCyclicBarrier(parties))

    ///
    /// Waits until all parties have invoked `await` on this barrier
    ///
    pub def awaitBarrier(barrier: CyclicBarrier): Int32 \ IO =
        let CyclicBarrier(b) = barrier;
        b.await()
}
