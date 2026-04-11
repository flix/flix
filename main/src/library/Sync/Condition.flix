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

pub mod Sync.Condition {

    use Sync.ReentrantLock.Lock

    ///
    /// An opaque portable condition handle bound to one reentrant lock.
    ///
    pub type alias Condition = ConditionHandle

    ///
    /// Errors returned by condition operations.
    ///
    pub enum ConditionError with Eq, ToString {
        case NotOwner
    }

    ///
    /// Returns a new condition bound to `lock`.
    ///
    pub def newCondition(lock: Lock): Condition \ IO =
        %%CONDITION_NEW%%(lock)

    ///
    /// Waits on `cond`, releasing its bound lock completely and reacquiring it before returning.
    ///
    /// Returns `Err(NotOwner)` if the current task does not own the bound lock.
    ///
    pub def await(cond: Condition): Result[ConditionError, Unit] \ IO =
        match %%CONDITION_AWAIT%%(cond) {
            case 0i32 => Ok(())
            case 1i32 => Err(ConditionError.NotOwner)
            case _ => bug!("unreachable")
        }

    ///
    /// Wakes one waiting task, if any.
    ///
    /// Returns `Err(NotOwner)` if the current task does not own the bound lock.
    ///
    pub def signal(cond: Condition): Result[ConditionError, Unit] \ IO =
        if (%%CONDITION_SIGNAL%%(cond)) Ok(()) else Err(ConditionError.NotOwner)

    ///
    /// Wakes all waiting tasks.
    ///
    /// Returns `Err(NotOwner)` if the current task does not own the bound lock.
    ///
    pub def signalAll(cond: Condition): Result[ConditionError, Unit] \ IO =
        if (%%CONDITION_SIGNAL_ALL%%(cond)) Ok(()) else Err(ConditionError.NotOwner)

}
