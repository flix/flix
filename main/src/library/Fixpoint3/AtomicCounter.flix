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
/// A thread-safe counter producing `Int64`.
/// 
mod Fixpoint3 {
    import java.util.concurrent.atomic.{AtomicLong => JAtomicLong}

    enum AtomicCounter[_: Region](JAtomicLong)

    mod AtomicCounter {
        import java.util.concurrent.atomic.{AtomicLong => JAtomicLong}
        use Fixpoint3.AtomicCounter

        ///
        /// Returns a fresh counter with initialized to `initialVal`.
        ///
        @Internal
        pub def mkCounter(_: Region[r], initialVal: Int64): AtomicCounter[r] \ r =
            AtomicCounter(unchecked_cast((new JAtomicLong(initialVal): _ \ IO) as _ \ r))

        ///
        /// Returns the current state of `counter` and increments it.
        ///
        @Internal
        pub def getAndIncrement(counter: AtomicCounter[r]): Int64 \ r =
            let AtomicCounter(c) = counter;
            unchecked_cast((c.getAndIncrement(): _ \ IO) as _ \ r)

        ///
        /// Returns the current state of `counter`.
        ///
        @Internal
        pub def get(counter: AtomicCounter[r]): Int64 \ r =
            let AtomicCounter(c) = counter;
            unchecked_cast((c.get(): _ \ IO) as _ \ r)
    }

}
