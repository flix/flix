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

pub mod Sync.CountDownLatch {

    ///
    /// An opaque portable count-down latch handle.
    ///
    pub type alias Latch = CountDownLatchHandle

    ///
    /// Errors returned by latch construction.
    ///
    pub enum LatchError with Eq, ToString {
        case InvalidCount
    }

    ///
    /// Returns a new count-down latch with the given non-negative count.
    ///
    pub def newLatch(count: Int32): Result[LatchError, Latch] \ IO =
        let latch = %%COUNT_DOWN_LATCH_NEW%%(count);
        if (Object.isNull(latch)) Err(LatchError.InvalidCount) else Ok(latch)

    ///
    /// Blocks cooperatively until the latch count reaches zero.
    ///
    pub def await(latch: Latch): Unit \ IO =
        %%COUNT_DOWN_LATCH_AWAIT%%(latch)

    ///
    /// Decrements the latch count, if positive.
    ///
    pub def countDown(latch: Latch): Unit \ IO =
        %%COUNT_DOWN_LATCH_COUNT_DOWN%%(latch)

}
