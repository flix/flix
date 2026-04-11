/*
 * Copyright 2026 Magnus Madsen
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

pub mod Sync.CyclicBarrier {

    ///
    /// An opaque portable cyclic barrier handle.
    ///
    pub type alias Barrier = CyclicBarrierHandle

    ///
    /// Errors returned by barrier construction.
    ///
    pub enum BarrierError with Eq, ToString {
        case InvalidParties
    }

    ///
    /// Returns a new cyclic barrier with the given positive party count.
    ///
    pub def newBarrier(parties: Int32): Result[BarrierError, Barrier] \ IO =
        let barrier = %%CYCLIC_BARRIER_NEW%%(parties);
        if (Object.isNull(barrier)) Err(BarrierError.InvalidParties) else Ok(barrier)

    ///
    /// Waits until the current barrier generation trips and returns the arrival index.
    ///
    /// The last arrival returns `0`. Earlier arrivals return increasing positive indices.
    ///
    pub def awaitBarrier(barrier: Barrier): Int32 \ IO =
        match %%CYCLIC_BARRIER_AWAIT%%(barrier) {
            case i if i >= 0i32 => i
            case _ => bug!("unreachable")
        }

}
