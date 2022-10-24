//
// Copyright 2018 Simon Meldahl Schmidt
// Copyright 2018-2021 Jonathan Lindegaard Starup
// Copyright 2021 Justin Fargnoli
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

namespace Concurrent/Channel {
    use Concurrent/ReentrantLock.lock
    use Concurrent/ReentrantLock.ReentrantLock
    use Concurrent/ReentrantLock.newLock
    use Concurrent/Condition.Condition
    use Concurrent/ReentrantLock.newCondition

    ///
    /// An enum that represents a multiple-producer, multiple-consumer (mpmc)
    /// channel.
    ///
    /// The tuple corresponds to (
    ///   `admin` - The administration info of the channel.
    ///   `elementDeque` - a deque of the channel elements but only used as a
    ///       queue.
    /// )
    ///
    /// -- Conditions and Signalling --
    /// When a channel needs to notify threads about available space or elements
    /// then every relevant thread is signalled. This means the if multiple
    /// threads are waiting to retrieve an element and an element arrives they
    /// all get woken up and most of them will again wait on a new element.
    ///
    /// -- Difference of `waitingGetters` and `waitingPutters` --
    /// The reason for the difference between `waitingGetters` and
    /// `waitingPutters` is the select expression. A thread can only wait for
    /// one condition which is a problem when a select expression requires
    /// waiting for a set of channels. Therefore the thread makes a condition
    /// that represents the select expression and hands that to every channel
    /// involved. put is not supported in select so a single condition can be
    /// reused for all insertions.
    ///
    /// -- Locks --
    /// Every condition is tied to a lock but a lock can exist on its own. The
    /// channel has a lock that needs to be held to operate on
    /// `elementDeque`, `size`, `waitingGetters` and `waitingPutters`. The
    /// `waitingPutters` condition is tied to the channel lock while the
    /// conditions in `waitingPutters` comes from locks created by the get
    /// function or the select function.
    ///
    /// -- Mpmc / MpmcAdmin split --
    /// The select function must work on heterogeneous channel collections,
    /// which cannot happen without existential types. This means that the
    /// non-polymorphic aspects relevant to select is split into its own
    /// structure on which select can operate.
    ///
    @Internal
    pub enum Mpmc[a](
        MpmcAdmin, // admin
        MutDeque[a, Static] // elementDeque
    )

    /// Returns the `MpmcAdmin` of `c`.
    @Internal
    pub def mpmcAdmin(c: Mpmc[a]): MpmcAdmin = {
        let Mpmc(admin, _) = c;
        admin
    }

    ///
    /// MpmcAdmin holds administrative information on the channel.
    ///
    /// The tuple corresponds to (
    ///   `id` - an unique id used to order the locking of sets of channels.
    ///       This avoids deadlocks.
    ///   `channelLock` - the lock that guards usage of `elementDeque`,
    ///       `waitingGetters` and `waitingPutters`.
    ///   `unBuffered` - If the channel is unbuffered then an insertion must
    ///       wait for a retrieval before continuing. A channel with a buffer
    ///       size of zero means that `unBuffered` is true and `maxSize`
    ///       is one. In every other case `unBuffered` is false and
    ///       `maxSize` corresponds to the buffer size.
    ///   `size` - the current number of elements in the channel. This is
    ///       here to support select on the non-polymorphic admin values.
    ///   `maxSize` - the maximum number of elements allowed in
    ///       `elementDeque`. This is strictly positive.
    ///   `waitingGetters` - a set of conditions that are waiting for an
    ///       element. This set is notified and cleared after each new element.
    ///       An available element is not guaranteed when notified.
    ///   `waitingPutters` - a condition that is waiting for an element.
    ///       The condition is notified after each new element. Available space
    ///       is not guaranteed when notified.
    /// )
    enum MpmcAdmin(
        Int64, // id
        ReentrantLock, // channelLock
        Bool, // unBuffered
        Int32, // maxSize
        Ref[Int32, Static], // size
        MutList[(ReentrantLock, Condition), Static], // waitingGetters
        Condition // waitingPutters
    )

    ///
    /// Creates a new channel. A runtime error occurs if `bufferSize` is
    /// negative. A channel with `bufferSize` zero means that sending and
    /// receiving is syncronized.
    ///
    @Internal
    pub def newChannel(bufferSize: Int32): Mpmc[a] \ IO =
        import static dev.flix.runtime.Global.newId(): Int64 \ IO;
        let _bufferCheck = if (bufferSize < 0) bug!("bufferSize < 0") else ();
        let unBuffered = bufferSize == 0;
        let reentrantLock = newLock(fairness());
        let size = ref 0;
        Mpmc(
            MpmcAdmin(
                newId(),
                reentrantLock,
                unBuffered,
                if (unBuffered) 1 else bufferSize,
                size,
                new MutList(Static),
                newCondition(reentrantLock)
            ),
            new MutDeque(Static)
        )

    ///
    /// Sends the element `e` on the channel `c`. This is blocking if the
    /// channel is full or unbuffered.
    ///
    /// Implements the expression `c <- e`.
    ///
    @Internal
    pub def put(e: a, c: Mpmc[a]): Unit \ IO =
        let Mpmc(admin, elementDeque) = c;
        let MpmcAdmin(_, channelLock, unBuffered, _, size, _, waitingPutters) = admin;
        lock(channelLock);

        // Block until the channel is not full
        awaitAvailableSpace(admin);

        // Insert the new element and update size
        MutDeque.pushBack(e, elementDeque);
        size := deref size + 1;

        // Signal the waiting getters that an element has arrived
        signalGetters(admin);

        // If the channel is unbuffered, wait for the element to be handed off
        // before continuing.
        /* TODO: Optimization possibility: This await could be faster with a
                 separate condition such that the hand-off signal does not have
                 to fight for the channel lock with the threads that actually
                 want to insert an element (unblocking from await requires that
                 you re-aquire the lock).
        */
        if (unBuffered) awaitCondition(waitingPutters)
        else ();

        unlock(channelLock)

    ///
    /// Retrieves an element from the channel `c`. This is blocking if the
    /// channel is empty.
    ///
    /// Implements to the expression `<- c`.
    ///
    @Internal
    pub def get(c: Mpmc[a]): a \ IO =
        let MpmcAdmin(_, channelLock, _, _, _, _, _) = mpmcAdmin(c);
        lock(channelLock);

        // Get the next available element
        let element = getHelper(c);

        unlock(channelLock);
        element

    ///
    /// Retrieves an element from the channel `c` and unlocks `locks` after doing so.
    /// Assumes the channel to be non-empty.
    ///
    @Internal
    pub def unsafeGetAndUnlock(c: Mpmc[a], locks: List[ReentrantLock]): a \ IO = {
        let Mpmc(admin, elementDeque) = c;
        let MpmcAdmin(_, channelLock, _, _, size, _, _) = admin;
        lock(channelLock);

        // Gets an element from the channel c which must be non-empty
        // and update the size.
        let optionalElement = MutDeque.popFront(elementDeque);
        size := deref size - 1;

        let value = match optionalElement {
            case None =>
                // The compiler should only use `unsafeGetAndUnlock` based on the
                // guaranteed output of `select`, so this should never happen.
                bug!("assertiveGet could not retrieve channel element")

            case Some(element) =>
                // Signal waiting setters that the channel has space
                signalPutters(admin);
                unlock(channelLock);
                element
        };

        // Unlock all the locks
        List.foreach(unlock, locks);

        value
    }

    ///
    /// Returns the index of the first channel that has an element in the array
    /// along with a set of locks to be unlocked after retrieval.
    /// Returns `(-1, _)` if no channels have elements and `blocking == false`,
    /// otherwise this blocks.
    /// OBS: In the blocking case, caller Must call `unlockLocks` on the
    ///      returned list of locks post call.
    ///      In the non-blocking case, selectFrom unlocks all locks first
    ///
    @Internal
    pub def selectFrom(
        channels: Array[MpmcAdmin, Static],
        blocking: Bool
    ): (Int32, List[ReentrantLock]) \ IO =
        // Create a new lock and condition for this select. The condition is
        // potentially put into the waiting getters of the channels if a
        // default isn't defined.
        let selectLock = newLock(fairness());
        let selectCondition = newCondition(selectLock);

        // Sort locks to avoid deadlocks
        let sortedLocks = channels |>
            Array.sortBy(match MpmcAdmin(id, _, _, _, _, _, _) -> id) |>
            Array.map(match MpmcAdmin(_, rlock, _, _, _, _, _) -> rlock);
        selectHelper(channels, blocking, sortedLocks, selectLock, selectCondition)

    ///
    /// The fairness policy of the locks.
    ///
    def fairness(): Bool = false

    ///
    /// Wait for the channel to have available space.
    /// The channel lock is expected to be held.
    ///
    @Internal
    def awaitAvailableSpace(c: MpmcAdmin): Unit \ IO =
        let MpmcAdmin(_, _, _, maxSize, size, _, waitingPutters) = c;
        if ((deref size) == maxSize) {
            awaitCondition(waitingPutters);
            awaitAvailableSpace(c)
        } else {
            ()
        }

    ///
    /// Recursive helper function for get, it repeatedly attempts to retrieve an
    /// element.
    /// 1) try to retrieve element, if so, return
    /// 2) wait for a new element
    /// 3) wake up, go to 1)
    ///
    /// The channel lock is expected to be held.
    ///
    def getHelper(c: Mpmc[a]): a \ IO =
        let Mpmc(admin, elementDeque) = c;
        let MpmcAdmin(_, channelLock, _, _, size, waitingGetters, _) = admin;

        // Try to get the element (which could already be taken by someone
        // else) and update size.
        let optionalElement = MutDeque.popFront(elementDeque);
        size := deref size - 1;

        match optionalElement {
            case None => { // No element was found
                // Create a new lock and condition
                let conditionLock = newLock(fairness());
                lock(conditionLock);

                let condition = newCondition(conditionLock);
                // Add Lock and Condition Pair to the channel
                MutList.push!((conditionLock, condition), waitingGetters);

                // Temporarily unlock the channel while waiting. This is
                // necessary as the condition comes from a different lock.
                unlock(channelLock);
                // We still hold the condition lock so there is no race here
                awaitCondition(condition);
                lock(channelLock);

                // Someone signalled that an element was put in the channel but
                // it might not be actually be there when we get to it.
                unlock(conditionLock);

                getHelper(c)
            }

            case Some(e) =>
                // Signal waiting setters that the channel has space
                signalPutters(admin);
                e
        }

    ///
    /// Recursive helper function for select, returns the index of the first
    /// channel that has an element in the array along with a set of locks to
    /// be unlocked after retrieval. Returns `(-1, _)` and unlocks
    /// `sortedLocks` if no channels have elements and `blocking == false`,
    /// otherwise this blocks.
    ///
    def selectHelper(
        channels: Array[MpmcAdmin, Static],
        blocking: Bool,
        sortedLocks: Array[ReentrantLock, Static],
        selectLock: ReentrantLock,
        selectCondition: Condition
    ): (Int32, List[ReentrantLock]) \ IO =
        // bug if the thread is interrupted
        if (threadInterrupted()) bug!("thread interrupted") else

        // Lock all locks in sorted order. This avoids the case
        // where a channel receives an element while the later channels are
        // searched for elements but before we place the condition on the
        // channels.
        Array.foreach(lock, sortedLocks);

        // Lock the select lock. Channels cannot signal the select condition
        // while this lock is held.
        lock(selectLock);

        // Find the first channel with an available element (channels are
        // searched in written order).
        /* TODO: Optimization opportunity. The order of searching here could be
                 randomized to avoid starvation scenarios).
        */
        let index = channels |>
            Array.mapWithIndex(i -> match MpmcAdmin(_, _, _, _, size, _, _) -> {
                (deref size > 0, i)
            }) |>
            Array.findLeft(match (b, _) -> b) |>
            Option.map(snd);

        match index {
            case Some(i) =>
                (i, Array.toList(sortedLocks) `List.append` (selectLock :: Nil))

            case None => { // No channel had an element ready
                if (blocking) { // We have to wait for an element
                    // Add our condition to all channels to get notified when a
                    // new element is added.
                    Array.foreach(addGetter(selectLock, selectCondition), channels);

                    // Unlock all channels in sorted order, so other threads
                    // may input elements. The order is not important for
                    // correctness.
                    Array.foreach(unlock, sortedLocks);

                    // Wait for an element to be added to any of the channels
                    awaitCondition(selectCondition);

                    // Unlock the selectLock
                    unlock(selectLock);

                    // Try again
                    selectHelper(channels, blocking, sortedLocks, selectLock, selectCondition)
                } else {
                    (Array.toList(sortedLocks) `List.append` (selectLock :: Nil)) |>
                        List.foreach(unlock);
                    (-1, Nil)
                }
            }
        }

    ///
    /// Add a condition to the list of waiting getters.
    /// The channel lock is expected to be held.
    ///
    def addGetter(l: ReentrantLock, cond: Condition, c: MpmcAdmin): Unit \ IO =
        let MpmcAdmin(_, _, _, _, _, waitingGetters, _) = c;
        MutList.push!((l, cond), waitingGetters)

    ///
    /// Signals and clears the waiting getters.
    /// The channel lock is expected to be held.
    ///
    def signalGetters(c: MpmcAdmin): Unit \ IO =
        let MpmcAdmin(_, _, _, _, _, waitingGetters, _) = c;

        // Signal waitingGetters that there is an element available
        let signalLockConditionPair = match (conditionLock, condition) -> {
            lock(conditionLock);
            signalCondition(condition);
            unlock(conditionLock)
        };

        MutList.foreach(signalLockConditionPair, waitingGetters);

        // Clear waitingGetters. If a waitingGetter does not receive an
        // element, it will add itself again
        MutList.clear!(waitingGetters)

    ///
    /// Signals and clears the waiting putters.
    /// The channel lock is expected to be held.
    ///
    def signalPutters(c: MpmcAdmin): Unit \ IO =
        let MpmcAdmin(_, _, _, _, _, _, waitingPutters) = c;
        // Signal waiting setters that the channel has space
        signalCondition(waitingPutters)
        // Since there is only one condition there is no cleaning up to do.


    // ------------- Helper Methods --------------------------------------------


    ///
    /// Wrapper for `java.lang.Thread:interrupted()`.
    ///
    def threadInterrupted(): Bool \ IO =
        import static java.lang.Thread.interrupted(): Bool \ IO;
        interrupted()

    ///
    /// Unsafe wrapper for `Concurrent/Condition.awaitUninterruptibly(c)` that
    /// handles `Err` with `bug!`.
    /// The channel lock is expected to be held.
    ///
    def awaitCondition(c: Condition): Unit \ IO =
        match Concurrent/Condition.awaitUninterruptibly(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Unsafe wrapper for `Concurrent/Condition.signalAll(c)` that handles
    /// `Err` with `bug!`.
    /// The condition lock is expected to be held.
    ///
    def signalCondition(c: Condition): Unit \ IO =
        match Concurrent/Condition.signalAll(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: awaiting without holding the corresponding lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Unsafe wrapper for `Concurrent/ReentrantLock.unlock(l)` that handles
    /// `Err` with `bug!`.
    /// The lock is expected to be held.
    ///
    def unlock(l: ReentrantLock): Unit \ IO =
        match Concurrent/ReentrantLock.unlock(l) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: unlock without holding the lock
                bug!("Implementation error: lock not held")
        }

}
