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

pub mod Sync.ReentrantLock {

    ///
    /// An opaque portable reentrant lock handle.
    ///
    pub type alias Lock = ReentrantLockHandle

    ///
    /// Errors returned by unlock.
    ///
    pub enum UnlockError with Eq, ToString {
        case NotOwner
    }

    ///
    /// Returns a new reentrant lock.
    ///
    pub def newLock(): Lock \ IO =
        %%REENTRANT_LOCK_NEW%%()

    ///
    /// Acquires `lock`, blocking cooperatively until it becomes available.
    ///
    pub def lock(lock: Lock): Unit \ IO =
        %%REENTRANT_LOCK_LOCK%%(lock)

    ///
    /// Attempts to acquire `lock` without blocking.
    ///
    pub def tryLock(lock: Lock): Bool \ IO =
        %%REENTRANT_LOCK_TRY_LOCK%%(lock)

    ///
    /// Releases `lock`.
    ///
    /// Returns `Err(NotOwner)` if the current task does not own the lock.
    ///
    pub def unlock(lock: Lock): Result[UnlockError, Unit] \ IO =
        if (%%REENTRANT_LOCK_UNLOCK%%(lock)) Ok(()) else Err(UnlockError.NotOwner)

}
