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

pub mod Sync.Semaphore {

    ///
    /// An opaque portable semaphore handle.
    ///
    pub type alias Sem = SemaphoreHandle

    ///
    /// Errors returned by semaphore construction.
    ///
    pub enum SemaphoreError with Eq, ToString {
        case InvalidPermits
    }

    ///
    /// Returns a new semaphore with the given non-negative initial permit count.
    ///
    pub def newSemaphore(permits: Int32): Result[SemaphoreError, Sem] \ IO =
        let sem = %%SEMAPHORE_NEW%%(permits);
        if (Object.isNull(sem)) Err(SemaphoreError.InvalidPermits) else Ok(sem)

    ///
    /// Acquires one permit, blocking cooperatively until one becomes available.
    ///
    pub def acquire(sem: Sem): Unit \ IO =
        %%SEMAPHORE_ACQUIRE%%(sem)

    ///
    /// Attempts to acquire one permit without blocking.
    ///
    pub def tryAcquire(sem: Sem): Bool \ IO =
        %%SEMAPHORE_TRY_ACQUIRE%%(sem)

    ///
    /// Releases one permit.
    ///
    pub def release(sem: Sem): Unit \ IO =
        %%SEMAPHORE_RELEASE%%(sem)

}
