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

namespace ChannelImpl {

    ///
    /// The tuple corresponds to (
    ///   `id` - an unique id used to order the locking of sets of channels. This avoids deadlocks,
    ///   `channelLock` - the lock of the channel,
    ///   `buffered` - indicates whether a handoff is needed or not.
    ///       If you try to put an element in a channel that's full, you wait until there's space,
    ///   `maxQueueSize` - the maximum number of elements allowed in `deque`. This is `max(1, channel size)`,
    ///   `deque` - a deque of the channel elements,
    ///   `waitingGetters` - a set of conditions that are waiting for an element.
    ///       This set is cleared after each new element,
    ///   `waitingSetters` - a condition that can notify threads of available space in `deque`.
    /// )
    /// The reason for the difference between `waitingGetters` and `waitingSetters` is the select expression.
    /// A thread can only wait for one condition which is a problem when a select expression requires waiting for a
    /// set of channels. Therefore the thread makes a condition that represents the select expression and hands that to
    /// every channel involved.
    ///
    /// put is not supported in select so a single condition can be reused for all insertions.
    ///
    pub enum ChannelImpl {
        case Mpmc(
                Int64,
                ReentrantLock,
                Bool,
                Int32,
                MutDeque[Boxed],
                MutList[(ReentrantLock, Condition)],
                Condition
            )
    }

    ///
    /// Equality is based on the channel `id` and is needed for `Order`.
    ///
    instance Eq[ChannelImpl] {
      pub def eq(x: ChannelImpl, y: ChannelImpl): Bool =
        let Mpmc(a_id, _, _, _, _, _, _) = x;
        let Mpmc(b_id, _, _, _, _, _, _) = y;
        a_id == b_id
    }


    ///
    /// Order is based on the channel `id` and is used by `selectImpl` to lock in a global ordering.
    ///
    instance Order[ChannelImpl] {
      pub def compare(x: ChannelImpl, y: ChannelImpl): Comparison =
        let Mpmc(a_id, _, _, _, _, _, _) = x;
        let Mpmc(b_id, _, _, _, _, _, _) = y;
        a_id <=> b_id
    }


    ///
    /// Creates a new channel. A runtime error occurs if `bufferSize` is negative.
    ///
    pub def new(bufferSize: Int32): ChannelImpl & Impure =
        import dev.flix.runtime.GlobalCounter:newId();
        let _bufferCheck = if (bufferSize < 0) bug!("bufferSize < 0") else ();
        let buffered = bufferSize == 0;
        let reentrantLock = ReentrantLock.new(false);
        Mpmc(
            newId(),
            reentrantLock,
            buffered,
            if (buffered) 1 else bufferSize,
            MutDeque.new(),
            MutList.new(),
            ReentrantLock.newCondition(reentrantLock)
        )

    ///
    /// Sends the element `e` on the channel `c`.
    ///
    /// Implements the expression `c <- e`.
    ///
    pub def put(c: ChannelImpl, e: Boxed): ChannelImpl & Impure =
        let Mpmc(_, channelLock, buffered, _, deque, waitingGetters, waitingSetters) = c;
        ReentrantLock.lock(channelLock);

        // Block until the channel is not full
        awaitAvailableSpace(c);

        // Insert the new element
        MutDeque.pushBack(e, deque);

        // Signal waitingGetters that there is an element available
        let signalLockConditionPair = lockConditionPair -> {
                let (conditionLock, condition) = lockConditionPair;
                ReentrantLock.lock(conditionLock);
                signalCondition(condition);
                unlockLock(conditionLock)
            };
        MutList.foreach(signalLockConditionPair, waitingGetters);

        // Clear waitingGetters.
        // If a waitingGetter does not receive an element, it will add itself again
        MutList.clear!(waitingGetters);

        // If the channel is unbuffered, wait for the element to be handed off before continuing
        // TODO: Optimization possibility. This await be faster with a separate condition such that the hand-off signal does not
        //     have to fight for the channel lock with the threads that actually want to insert an element.
        if (buffered) awaitCondition(waitingSetters)
        else ();

        unlockLock(channelLock);
        c

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Implements to the expression `<- c`.
    ///
    pub def get(c: ChannelImpl): Boxed & Impure =
        let Mpmc(_, channelLock, _, _, deque, _, waitingSetters) = c;
        ReentrantLock.lock(channelLock);
        // Pop the next element
        let optionalElement = MutDeque.popFront(deque);
        let element = getHelper(c, optionalElement);

        // Signal waiting setters that the channel has space
        signalCondition(waitingSetters);

        unlockLock(channelLock);
        element

    ///
    /// Returns the first channel that has an element in the array along with its index.
    ///
    pub def selectImpl(channels: Array[ChannelImpl], hasDefault: Bool): Option[(Int32, Boxed)] & Impure =
        // Create new Condition and channelLock the current thread
        let selectLock = ReentrantLock.new(false);
        let selectCondition = ReentrantLock.newCondition(selectLock);

        // Sort channels to avoid deadlocks
        let sortedChannels = Array.sort(channels);
        selectHelper(channels, sortedChannels, selectLock, selectCondition, hasDefault)

    ///
    /// Wait for the channel to have available space.
    /// The channel lock is expected to be held.
    ///
    def awaitAvailableSpace(c: ChannelImpl): Unit & Impure =
        let Mpmc(_, _, _, maxQueueSize, deque, _, waitingSetters) = c;
        if (MutDeque.size(deque) == maxQueueSize) {
            awaitCondition(waitingSetters);
            awaitAvailableSpace(c)
        } else {
            ()
        }

    ///
    /// Recursive helper function for get, it repeatedly attempts to retrieve an element.
    /// The channel lock is expected to be held.
    ///
    def getHelper(c: ChannelImpl, o: Option[Boxed]): Boxed & Impure =
        match o {
            case None => {
                let Mpmc(_, channelLock, _, _, deque, waitingGetters, _) = c;
                // No element was found

                // Create a new Lock and Condition
                let conditionLock = ReentrantLock.new(false);
                ReentrantLock.lock(conditionLock);

                let condition = ReentrantLock.newCondition(conditionLock);
                // Add LockConditionPair to the channel
                let pair = (conditionLock, condition);

                // Add LockConditionPair to the channel
                MutList.push!(pair, waitingGetters);

                // Temporarily unlock the channel while waiting. This is necessary as the Condition comes from a different Lock.
                unlockLock(channelLock);
                // We still hold the condition lock so there is no race here
                awaitCondition(condition);
                ReentrantLock.lock(channelLock);

                // Someone signalled that an element was put in the channel.
                // Try to get the element (which could already be taken by someone else)
                let optionalElement = MutDeque.popFront(deque);

                unlockLock(conditionLock);

                getHelper(c, optionalElement)
            }
            case Some(e) => e
        }

    ///
    /// Try to retrieve the first element and if the channel is empty, return `None`.
    ///
    def tryGet(c: ChannelImpl): Option[Boxed] & Impure =

        let Mpmc(_, channelLock, _, _, deque, _, waitingSetters) = c;
        // TODO: This lock seems redundant since it is already held in selectHelper that calls firstAvailableElement that calls this
        ReentrantLock.lock(channelLock);

        // Try to get an element from the channel
        let element = MutDeque.popFront(deque);

        // If there was an element, signal waiting setters
        match element {
            case None => ()
            case _ => signalCondition(waitingSetters)
        };

        unlockLock(channelLock);
        // Return the element from the channel, or None if channel was empty
        element

    ///
    /// Recursive helper function for selectImpl, returns the first channel that has an element
    /// in the array along with its index.
    ///
    def selectHelper(channels: Array[ChannelImpl], sortedChannels: Array[ChannelImpl], selectLock: ReentrantLock, selectCondition: Condition, hasDefault: Bool): Option[(Int32, Boxed)] & Impure =
        if (not threadInterrupted()) {
            // Lock all channels in sorted order
            let lockChannel = c -> {
                let Mpmc(_, lock, _, _, _, _, _) = c;
                ReentrantLock.lock(lock)
            };
            Array.foreach(lockChannel, sortedChannels);

            // Lock the select lock after the channels
            ReentrantLock.lock(selectLock);

            // Find channels with waiting elements
            let selectChoice = firstAvailableElement(0, channels);

            let unlockChannel = c -> {
                let Mpmc(_, lock, _, _, _, _, _) = c;
                unlockLock(lock)
            };

            match selectChoice {
                case Some((i, e)) => {
                    // Unlock all channels in sorted order, so other threads may input elements
                    Array.foreach(unlockChannel, sortedChannels);

                    // Unlock the selectLock
                    unlockLock(selectLock);

                    Some((i, e))
                }
                case None => {
                    // No channel had an element ready
                    if (hasDefault) {
                        // we can return None to indicate the default value
                        // Unlock all channels in sorted order, so other threads may input elements
                        Array.foreach(unlockChannel, sortedChannels);

                        // Unlock the selectLock
                        unlockLock(selectLock);

                        None
                    } else {
                        // We have to wait for an element
                        // Add our condition to all channels to get notified when a new element is added
                        let addGetterToChannel = c -> {
                            let pair = (selectLock, selectCondition);
                            let Mpmc(_, _, _, _, _, waitingGetters, _) = c;

                            // Add LockConditionPair to the channel
                            MutList.push!(pair, waitingGetters)
                        };
                        Array.foreach(addGetterToChannel, channels);

                        // Unlock all channels in sorted order, so other threads may input elements
                        Array.foreach(unlockChannel, sortedChannels);

                        // Wait for an element to be added to any of the channels
                        awaitCondition(selectCondition);

                        // Unlock the selectLock
                        unlockLock(selectLock);

                        // try again
                        selectHelper(channels, sortedChannels, selectLock, selectCondition, hasDefault)
                    }
                }
            }
        } else {
            bug!("thread interrupted")
        }

    ///
    /// Retrieves the element of the first channel in the array that has one, if such a channel exists.
    ///
    def firstAvailableElement(i: Int32, channels: Array[ChannelImpl]): Option[(Int32, Boxed)] & Impure =
        if (i < 0 or i >= Array.length(channels)) {
            // Passed `index` out of bounds
            bug!("Implementation error: out of bounds ${i}, length ${Array.length(channels)}")
        } else {
            match tryGet(channels[i]) {
                case None =>
                    // try the next channel
                    if (i == Array.length(channels)-1) None
                    else firstAvailableElement(i + 1, channels)
                case Some(element) => {
                    // There is a waiting element in this channel.
                    // Return the element and the index of this channel
                    Some((i, element))
                }
            }
        }


    // ----- Helper Methods -----

    ///
    /// Wrapper for `java.lang.Thread:interrupted()`.
    ///
    def threadInterrupted(): Bool & Impure =
        import java.lang.Thread:interrupted();
        interrupted()

    ///
    /// Wrapper for `Condition.awaitUninterruptibly(c)` that handles `Err` with `bug!`
    /// The channel lock is expected to be held.
    ///
    def awaitCondition(c: Condition): Unit & Impure =
        match Condition.awaitUninterruptibly(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Wrapper for `Condition.signalAll(c)` that handles `Err` with `bug!`
    /// The condition lock is expected to be held.
    ///
    def signalCondition(c: Condition): Unit & Impure =
        match Condition.signalAll(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                bug!("Implementation error: lock not held")
        }

    ///
    /// Wrapper for `ReentrantLock.unlock(l)` that handles `Err` with `bug!`
    /// The lock is expected to be held.
    ///
    def unlockLock(l: ReentrantLock): Unit & Impure =
        match ReentrantLock.unlock(l) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: unlock without holding the lock
                bug!("Implementation error: lock not held")
        }

}
