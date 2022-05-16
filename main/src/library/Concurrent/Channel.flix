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

    ///
    /// An enum that represents a multiple-producer, multiple-consumer (mpmc)
    /// channel.
    ///
    /// The tuple corresponds to (
    ///   `id` - an unique id used to order the locking of sets of channels.
    ///       This avoids deadlocks.
    ///   `channelLock` - the lock that guards usage of `elementDeque`,
    ///       `waitingGetters` and `waitingPutters`.
    ///   `unBuffered` - If the channel is unbuffered then an insertion must
    ///       wait for a retrieval before continuing. A channel with a buffer
    ///       size of zero means that `unBuffered` is true and `maxQueueSize`
    ///       is one. In every other case `unBuffered` is false and
    ///       `maxQueueSize` corresponds to the buffer size.
    ///   `maxQueueSize` - the maximum number of elements allowed in
    ///       `elementDeque`. This is strictly positive.
    ///   `elementDeque` - a deque of the channel elements but only used as a
    ///       queue.
    ///   `waitingGetters` - a set of conditions that are waiting for an
    ///       element. This set is notified and cleared after each new element.
    ///       An available element is not guaranteed when notified.
    ///   `waitingPutters` - a condition that is waiting for an element.
    ///       The condition is notified after each new element. Available space
    ///       is not guaranteed when notified.
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
    /// `elementDeque`, `waitingGetters` and `waitingPutters`. The
    /// `waitingPutters` condition is tied to the channel lock while the
    /// conditions in `waitingPutters` comes from locks created by the get
    /// function or the select function.
    ///
    pub enum Mpmc {
        case Mpmc(
            Int64, // id
            Concurrent/ReentrantLock.ReentrantLock, // channelLock
            Bool, // unBuffered
            Int32, // maxQueueSize
            MutDeque[Boxed, Impure], // elementDeque
            MutList[(Concurrent/ReentrantLock.ReentrantLock, Concurrent/Condition.Condition), Impure], // waitingGetters
            Concurrent/Condition.Condition // waitingPutters
        )
    }

    ///
    /// Equality is based on the channel `id` and is needed for the `Order`
    /// instance.
    ///
    instance Eq[Mpmc] {
        pub def eq(x: Mpmc, y: Mpmc): Bool =
        let Mpmc(a_id, _, _, _, _, _, _) = x;
        let Mpmc(b_id, _, _, _, _, _, _) = y;
        a_id == b_id
    }

    ///
    /// Order is based on the channel `id` and is used by `select` to lock
    /// in a global ordering.
    ///
    instance Order[Mpmc] {
        pub def compare(x: Mpmc, y: Mpmc): Comparison =
        let Mpmc(a_id, _, _, _, _, _, _) = x;
        let Mpmc(b_id, _, _, _, _, _, _) = y;
        a_id <=> b_id
    }

    ///
    /// Creates a new channel. A runtime error occurs if `bufferSize` is
    /// negative. A channel with `bufferSize` zero means that sending and
    /// receiving is syncronized.
    ///
    @Internal
    pub def new(bufferSize: Int32): Mpmc & Impure =
        import static dev.flix.runtime.Global.newId(): Int64 & Impure;
        let _bufferCheck = if (bufferSize < 0) bug!("bufferSize < 0") else ();
        let unBuffered = bufferSize == 0;
        let reentrantLock = Concurrent/ReentrantLock.new(false);
        Mpmc(
            newId(),
            reentrantLock,
            unBuffered,
            if (unBuffered) 1 else bufferSize,
            new MutDeque(static),
            new MutList(static),
            Concurrent/ReentrantLock.newCondition(reentrantLock)
        )

    ///
    /// Sends the element `e` on the channel `c`. This is blocking if the
    /// channel is full or unbuffered.
    ///
    /// Implements the expression `c <- e`.
    ///
    @Internal
    pub def put(e: Boxed, c: Mpmc): Unit & Impure =
        let Mpmc(_, channelLock, unBuffered, _, elementDeque, _, waitingPutters) = c;
        lock(channelLock);

        // Block until the channel is not full
        awaitAvailableSpace(c);

        // Insert the new element
        MutDeque.pushBack(e, elementDeque);

        // Signal the waiting getters that an element has arrived
        signalGetters(c);

        // If the channel is unbuffered, wait for the element to be handed off
        // before continuing.
        // TODO: Optimization possibility. This await could be faster with a
        //     separate condition such that the hand-off signal does not have
        //     to fight for the channel lock with the threads that actually
        //     want to insert an element (unblocking from await requires that
        //     you re-aquire the lock).
        if (unBuffered) awaitCondition(waitingPutters)
        else ();

        unlock(channelLock)

    ///
    /// Receives an element from the channel `c`. This is blocking if the
    /// channel is empty.
    ///
    /// Implements to the expression `<- c`.
    ///
    @Internal
    pub def get(c: Mpmc): Boxed & Impure =
        let Mpmc(_, channelLock, _, _, _, _, _) = c;
        lock(channelLock);

        // Get the next available element
        let element = getHelper(c);

        unlock(channelLock);
        element

    ///
    /// Returns the first channel that has an element in the array along with
    /// the index of the channel. Returns None if the default case is chosen.
    ///
    @Internal
    pub def select(channels: Array[Mpmc], hasDefault: Bool): Option[(Int32, Boxed)] & Impure =
        // Create a new lock and condition for this select. The condition is
        // potentially put into the waiting getters of the channels if a
        // default isn't defined.
        let selectLock = Concurrent/ReentrantLock.new(false);
        let selectCondition = Concurrent/ReentrantLock.newCondition(selectLock);

        // Sort channels to avoid deadlocks
        let sortedChannels = Array.sort(channels);
        selectHelper(channels, hasDefault, sortedChannels, selectLock, selectCondition)

    ///
    /// Wait for the channel to have available space.
    /// The channel lock is expected to be held.
    ///
    @Internal
    def awaitAvailableSpace(c: Mpmc): Unit & Impure =
        let Mpmc(_, _, _, maxQueueSize, elementDeque, _, waitingPutters) = c;
        if (MutDeque.size(elementDeque) == maxQueueSize) {
            awaitCondition(waitingPutters);
            awaitAvailableSpace(c)
        } else {
            ()
        }

    ///
    /// Recursive helper function for get, it repeatedly attempts to retrieve an
    /// element.
    /// The channel lock is expected to be held.
    ///
    def getHelper(c: Mpmc): Boxed & Impure =
        let Mpmc(_, channelLock, _, _, elementDeque, waitingGetters, _) = c;
        // Try to get the element (which could already be taken by someone
        // else)
        let optionalElement = MutDeque.popFront(elementDeque);
        match optionalElement {
            case None => { // No element was found
                // Create a new lock and condition
                let conditionLock = Concurrent/ReentrantLock.new(false);
                lock(conditionLock);

                let condition = Concurrent/ReentrantLock.newCondition(conditionLock);
                // Add LockConditionPair to the channel
                let pair = (conditionLock, condition);

                // Add LockConditionPair to the channel
                MutList.push!(pair, waitingGetters);

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
                signalPutters(c);
                e
        }

    ///
    /// Try to retrieve the first element and if the channel is empty, return
    /// `None`.
    ///
    def tryGet(c: Mpmc): Option[Boxed] & Impure =
        let Mpmc(_, channelLock, _, _, elementDeque, _, _) = c;
        // TODO: Optimization possibility. This lock seems redundant since it
        // is already held in selectHelper that calls firstAvailableElement
        // that calls this.
        lock(channelLock);

        // Try to get an element from the channel
        let element = MutDeque.popFront(elementDeque);

        // If there was an element, signal waiting setters
        Option.foreach(_ -> signalPutters(c), element);

        unlock(channelLock);
        element

    ///
    /// Recursive helper function for select, returns the first channel that
    /// has an element in the array along with its index. Returns false if the
    /// default case is chosen (no element was immediately available).
    ///
    def selectHelper(channels: Array[Mpmc], hasDefault: Bool, sortedChannels: Array[Mpmc], selectLock: Concurrent/ReentrantLock.ReentrantLock, selectCondition: Concurrent/Condition.Condition): Option[(Int32, Boxed)] & Impure =
        // bug if the thread is interrupted
        if (threadInterrupted()) bug!("thread interrupted") else

        // Lock all channels in sorted order. This avoids the case
        // where a channel receives an element while the later channels are
        // searched for elements but before we place the condition on the
        // channels.
        lockChannels(sortedChannels);

        // Lock the select lock. Channels cannot signal the select condition
        // while this lock is held.
        lock(selectLock);

        // Find the first channel with an availble element (channels is used
        // here to probe the channels in written order).
        // TODO: Optimization opportunity. The order of searching here could be
        //     randomized to avoid starvation scenarios).
        let selectChoice = firstAvailableElement(0, channels);

        match selectChoice {
            case Some((i, e)) => { // We found an element and can return
                // Unlock all channels in sorted order, so other threads may
                // input elements. The order is not important for correctness.
                unlockChannels(sortedChannels);

                // Unlock the selectLock
                unlock(selectLock);

                Some((i, e))
            }

            case None => { // No channel had an element ready
                if (hasDefault) { // We can return the default case
                    // Unlock all channels in sorted order, so other threads
                    // may input elements. The order is not important for
                    // correctness.
                    unlockChannels(sortedChannels);

                    // Unlock the selectLock
                    unlock(selectLock);

                    None
                } else { // We have to wait for an element
                    // Add our condition to all channels to get notified when a
                    // new element is added.
                    Array.foreach(addGetter(selectLock, selectCondition), channels);

                    // Unlock all channels in sorted order, so other threads
                    // may input elements. The order is not important for
                    // correctness.
                    unlockChannels(sortedChannels);

                    // Wait for an element to be added to any of the channels
                    awaitCondition(selectCondition);

                    // Unlock the selectLock
                    unlock(selectLock);

                    // Try again
                    selectHelper(channels, hasDefault, sortedChannels, selectLock, selectCondition)
                }
            }
        }

    ///
    /// Locks the given channels.
    ///
    def lockChannels(a: Array[Mpmc]): Unit & Impure =
        let lockChannel = match Mpmc(_, channelLock, _, _, _, _, _) -> lock(channelLock);
        Array.foreach(lockChannel, a)

    ///
    /// Unlocks the given channels.
    ///
    def unlockChannels(a: Array[Mpmc]): Unit & Impure =
        let unlockChannel = match Mpmc(_, lock, _, _, _, _, _) -> unlock(lock);
        // TODO: Optimization possibility. Golang unlocks their channels in
        // reverse order.
        // performance.
        Array.foreach(unlockChannel, a)

    ///
    /// Retrieves the element of the first channel in the array that has one,
    /// if such a channel exists. A runtime error ocurrs if i is out of bounds.
    ///
    def firstAvailableElement(i: Int32, channels: Array[Mpmc]): Option[(Int32, Boxed)] & Impure =
        if (i < 0 or i >= Array.length(channels)) {
            // i is out of bounds
            bug!("Implementation error: out of bounds ${i}, length ${Array.length(channels)}")
        } else {
            match tryGet(channels[i]) {
                case None =>
                    // Try the next channel if there is one
                    if (i == Array.length(channels)-1) None
                    else firstAvailableElement(i + 1, channels)
                case Some(element) => {
                    // There is a waiting element in this channel.
                    // Return the element and the index of this channel
                    Some((i, element))
                }
            }
        }

    ///
    /// Add a condition to the list of waiting getters.
    /// The channel lock is expected to be held.
    ///
    def addGetter(l: Concurrent/ReentrantLock.ReentrantLock, cond: Concurrent/Condition.Condition, c: Mpmc): Unit & Impure =
        let pair = (l, cond);
        let Mpmc(_, _, _, _, _, waitingGetters, _) = c;

        // Add the pair to the channel
        MutList.push!(pair, waitingGetters)


    ///
    /// Signals and clears the waiting getters.
    /// The channel lock is expected to be held.
    ///
    def signalGetters(c: Mpmc): Unit & Impure =
        let Mpmc(_, _, _, _, _, waitingGetters, _) = c;

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
    /// Signals and clears the waiting setters.
    /// The channel lock is expected to be held.
    ///
    def signalPutters(c: Mpmc): Unit & Impure =
        let Mpmc(_, _, _, _, _, _, waitingPutters) = c;

        // Signal waiting setters that the channel has space
        signalCondition(waitingPutters)
        // Since there is only one condition there is no cleaning up to do.

    // ----- Helper Methods -----

    ///
    /// Wrapper for `java.lang.Thread:interrupted()`.
    ///
    def threadInterrupted(): Bool & Impure =
        import static java.lang.Thread.interrupted(): Bool & Impure;
        interrupted()

    ///
    /// Wrapper for `Concurrent/Condition.awaitUninterruptibly(c)` that handles `Err` with
    /// `bug!`.
    /// The channel lock is expected to be held.
    ///
    def awaitCondition(c: Concurrent/Condition.Condition): Unit & Impure =
        match Concurrent/Condition.awaitUninterruptibly(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Wrapper for `Concurrent/Condition.signalAll(c)` that handles `Err` with `bug!`.
    /// The condition lock is expected to be held.
    ///
    def signalCondition(c: Concurrent/Condition.Condition): Unit & Impure =
        match Concurrent/Condition.signalAll(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Wrapper for `Concurrent/ReentrantLock.unlock(l)` that handles `Err` with `bug!`.
    /// The lock is expected to be held.
    ///
    def unlock(l: Concurrent/ReentrantLock.ReentrantLock): Unit & Impure =
        match Concurrent/ReentrantLock.unlock(l) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: unlock without holding the lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Wrapper for `Concurrent/ReentrantLock.lock(l)`.
    ///
    def lock(l: Concurrent/ReentrantLock.ReentrantLock): Unit & Impure =
        Concurrent/ReentrantLock.lock(l)

}
