/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fixpoint3.Counter {

    ///
    /// `Counter` is a wrapper around `Ref` for a simple counter.
    ///
    /// Its primary use is for generating identifiers.
    ///
    /// Not thread-safe.
    ///
    pub struct Counter[r: Region] {
        mut state: Int32
    }

    ///
    /// Returns a fresh `Counter` initialized to `0`.
    ///
    pub def fresh(rc: Region[r]): Counter[r] \ r = new Counter @ rc {state = 0}

    ///
    /// Returns the state of `counter` and increments it.
    ///
    pub def getAndIncrement(counter: Counter[r]): Int32 \ r =
        let newId = counter->state;
        counter->state = newId + 1;
        newId

    ///
    /// Increments the state of `counter`.
    ///
    pub def increment(counter: Counter[r]): Unit \ r =
        discard getAndIncrement(counter)

    ///
    /// Returns the state of `counter`.
    ///
    pub def get(counter: Counter[r]): Int32 \ r =
        counter->state

    ///
    /// Sets the state of `counter` to `i`.
    ///
    pub def set(i: Int32, counter: Counter[r]): Unit \ r =
        counter->state = i
}
