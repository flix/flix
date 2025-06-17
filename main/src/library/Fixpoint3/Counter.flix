/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

///
/// `Counter` is a wrapper around `Ref` for a simple counter.
///
/// Its primary use is for generating identifiers.
///
/// Not thread-safe.
///
mod Fixpoint3 {
    struct Counter[r: Region] {
        mut state: Int32
    }

    mod Counter {
        use Fixpoint3.Counter

        ///
        /// Returns a fresh `Counter` initialized to `0`.
        ///
        @Internal
        pub def fresh(rc: Region[r]): Counter[r] \ r = new Counter @ rc {state = 0}

        ///
        /// Returns the state of `counter` and increments it.
        ///
        @Internal
        pub def getAndIncrement(counter: Counter[r]): Int32 \ r =
            let newId = counter->state;
            counter->state = newId + 1;
            newId

        ///
        /// Increments the state of `counter`.
        ///
        @Internal
        pub def increment(counter: Counter[r]): Unit \ r =
            getAndIncrement(counter);
            ()

        ///
        /// Returns the state of `counter`.
        ///
        @Internal
        pub def get(counter: Counter[r]): Int32 \ r =
            counter->state

        ///
        /// Sets the state of `counter` to `i`.
        ///
        @Internal
        pub def set(i: Int32, counter: Counter[r]): Unit \ r =
            counter->state = i
    }

}
