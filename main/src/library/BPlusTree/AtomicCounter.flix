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

pub mod BPlusTree.AtomicCounter {
    import java.util.concurrent.atomic.{LongAdder => JLongAdder}

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
