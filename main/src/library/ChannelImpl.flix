/*
 * Copyright 2018 Simon Meldahl Schmidt
 * Copyright 2018-2021 Jonathan Lindegaard Starup
 * Copyright 2021 Justin Fargnoli
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

namespace ChannelImpl {

    /**
    * The tuple corresponds to (id, channelLock, bufferType, elementQueue, waitingGetters, waitingSetters)
    *   - id
    *   - channelLock is the channelLock of this channel.
    *   - buffered indicates whether a handoff is needed or not. If you try to put an element in a channel that's full, you wait until there's space.
    *   - maxQueueSize is the max elements allowed in the queue. This is max(1, channel size).
    *   - deque is a circular buffer of elements in the list, if bufferSize is 0 the the queue is size 1.
    *   - waitingGetters is a set of conditions that is waiting for get. This set is cleared after each new element.
    *   - waitingSetters is a condition that can notify threads of available space in the elementQueue.
    */
    pub enum ChannelImpl {
        case Mpmc(
                Int64,
                ReentrantLock,
                Bool,
                Int,
                MutDeque[Boxed],
                MutList[(ReentrantLock, Condition)],
                Condition
            )
    }

    pub def new(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        if (bufferSize < 0) Err("Channel's buffer size must be non-negative.")
        else Ok(unsafeNew(bufferSize))

    pub def unsafeNew(bufferSize: Int): ChannelImpl & Impure =
        import dev.flix.runtime.GlobalCounter:newId();
        let _bufferCheck = if (bufferSize < 0) inputBug("newChannelWithQueue", "bufferSize < 0") else ();
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
    /// Sends the element `x` on the channel `c`.
    ///
    /// Implements the expression `c <- x`.
    ///
    pub def put(channel: ChannelImpl, boxedElement: Boxed): ChannelImpl & Impure =
        let Mpmc(_, channelLock, buffered, _, deque, waitingGetters, waitingSetters) = channel;
        ReentrantLock.lock(channelLock);

        // Block until the channel is not full
        awaitAvailableSpace(channel);

        // Insert the new element
        MutDeque.pushBack(boxedElement, deque);

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
        // TODO This might be faster with a separate condition such that the hand-off signal does not
        // have to fight for the channel lock.
        if (buffered) awaitCondition(waitingSetters)
        else ();

        unlockLock(channelLock);
        channel

    ///
    /// The channel lock is expected to be held.
    ///
    def awaitAvailableSpace(channel: ChannelImpl): Unit & Impure =
        let Mpmc(_, _, _, maxQueueSize, deque, _, waitingSetters) = channel;
        if (MutDeque.size(deque) == maxQueueSize) {
            awaitCondition(waitingSetters);
            awaitAvailableSpace(channel)
        } else {
            ()
        }

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Implements to the expression `<- c`.
    ///
    pub def get(channel: ChannelImpl): Boxed & Impure =
        let Mpmc(_, channelLock, _, _, deque, _, waitingSetters) = channel;
        ReentrantLock.lock(channelLock);
        // Pop the next element
        let optionalElement = MutDeque.popFront(deque);
        let element = getHelper(channel, optionalElement);

        // Signal waiting setters that the channel has space
        signalCondition(waitingSetters);

        unlockLock(channelLock);
        element

    ///
    /// Recursive helper function for get, loops and awaits on empty channel.
    /// The channel lock is expected to be held.
    ///
    def getHelper(channel: ChannelImpl, element: Option[Boxed]): Boxed & Impure =
        match element {
            case None => {
                let Mpmc(_, channelLock, _, _, deque, waitingGetters, _) = channel;
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

                getHelper(channel, optionalElement)
            }
            case Some(e) => e
        }

    def tryGet(channel: ChannelImpl): Option[Boxed] & Impure =

        let Mpmc(_, channelLock, _, _, deque, _, waitingSetters) = channel;
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

    def cmp(a: ChannelImpl, b: ChannelImpl): Comparison =
        let Mpmc(a_id, _, _, _, _, _, _) = a;
        let Mpmc(b_id, _, _, _, _, _, _) = b;
        a_id <=> b_id

    /**
    * Given a array of channels, returns the first channel that has an element
    * and return the index of that channel and the retrieved element in a
    * SelectChoice object.
    *
    * @param channels the channels to select on
    * @return the channel index of the channel with an element and the element
    */
    pub def selectImpl(channels: Array[ChannelImpl], hasDefault: Bool): Option[(Int, Boxed)] & Impure =
        // Create new Condition and channelLock the current thread
        let selectLock = ReentrantLock.new(false);
        let selectCondition = ReentrantLock.newCondition(selectLock);

        // Sort channels to avoid deadlocks
        let sortedChannels = Array.sortWith(cmp, channels);
        selectHelper(channels, sortedChannels, selectLock, selectCondition, hasDefault)

    def selectHelper(channels: Array[ChannelImpl], sortedChannels: Array[ChannelImpl], selectLock: ReentrantLock, selectCondition: Condition, hasDefault: Bool): Option[(Int, Boxed)] & Impure =
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
            internalBug("selectHelper", "thread interrupted")
        }

    def firstAvailableElement(index: Int, channels: Array[ChannelImpl]): Option[(Int, Boxed)] & Impure =
        if (index < 0 or index >= Array.length(channels)) {
            // Passed `index` out of bounds
            internalBug("firstAvailableElement", "out of bounds ${index}, length ${Array.length(channels)}")
        } else {
            match tryGet(channels[index]) {
                case None =>
                    // try the next channel
                    if (index == Array.length(channels)-1) None
                    else firstAvailableElement(index + 1, channels)
                case Some(element) => {
                    // There is a waiting element in this channel.
                    // Return the element and the index of this channel
                    Some((index, element))
                }
            }
        }


    // ----- Helper Methods -----

    def internalBug(methodName: String, description: String): a =
        bug!("implementation error in ChannelImpl.flix.${methodName}, ${description}")

    def inputBug(methodName: String, description: String): a =
        bug!("ChannelImpl.${methodName}, ${description}")

    def threadInterrupted(): Bool & Impure =
        import java.lang.Thread:interrupted();
        interrupted()

    ///
    /// The channel lock is expected to be held.
    ///
    def awaitCondition(c: Condition): Unit & Impure =
        match Condition.awaitUninterruptibly(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                internalBug("awaitCondition", "lock not held")
        }

    ///
    /// The condition lock is expected to be held.
    ///
    def signalCondition(c: Condition): Unit & Impure =
        match Condition.signalAll(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await without holding the corresponding lock
                internalBug("signalCondition", "lock not held")
        }

    ///
    /// The lock is expected to be held.
    ///
    def unlockLock(l: ReentrantLock): Unit & Impure =
        match ReentrantLock.unlock(l) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: unlock without holding the lock
                internalBug("unlockLock", "lock not held")
        }

}
