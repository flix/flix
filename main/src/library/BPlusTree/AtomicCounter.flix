/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod BPlusTree.AtomicCounter {
    import java.util.concurrent.atomic.{LongAdder => JLongAdder}

    ///
    /// A thread-safe counter producing `Int32`.
    ///
    /// A `LongAdder` is used as `AtomicInteger`/`AtomicLong` appeared to perform
    /// badly under high contention.
    ///
    pub enum AtomicCounter[_: Region](JLongAdder)

    ///
    /// Returns a fresh counter with initialized to `initialVal`.
    ///
    pub def mkCounter(_: Region[r], initialValue: Int32): AtomicCounter[r] \ r =
        unsafe IO as r {
            let adder = new JLongAdder();
            adder.add(Int32.toInt64(initialValue));
            AtomicCounter(adder)
        }

    ///
    /// Increments `counter` by `1`.
    ///
    pub def increment(counter: AtomicCounter[r]): Unit \ r =
        let AtomicCounter(c) = counter;
        unsafe IO as r { c.increment() }

    ///
    /// Returns the current state of `counter`.
    ///
    pub def get(counter: AtomicCounter[r]): Int32 \ r =
        let AtomicCounter(c) = counter;
        unsafe IO as r { c.intValue() }
}
