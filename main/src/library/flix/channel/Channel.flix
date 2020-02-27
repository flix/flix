/*
 * Copyright 2019 Magnus Madsen
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

namespace Channel {

    ///
    /// Returns a fresh unbuffered channel.
    ///
    /// An unbuffered channel has an internal capacity of `0`.
    ///
    pub def new(): Channel[a] & Impure = chan a 0

    ///
    /// Returns a fresh buffered channel with internal capacity `c`.
    ///
    pub def newWithCapacity(c: Int): Channel[a] & Impure = chan a c

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Equivalent to the expression `<- c`.
    ///
    pub def get(c: Channel[a]): a & Impure = <- c

    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Equivalent to the expression `c <- x`.
    ///
    pub def put(c: Channel[a], x: a): Channel[a] & Impure = c <- x

}
