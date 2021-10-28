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
    *   - typedDeque is a circular buffer of elements in the list, if bufferSize is 0 the the queue is size 1.
    *   - waitingGetters is a set of conditions that is waiting for get. This set is cleared after each new element.
    *   - waitingSetters is a condition that can notify threads of available space in the elementQueue.
    */
    pub enum ChannelImpl {
        case Mpmc(
                Int64,
                ReentrantLock,
                Bool,
                Int,
                TypedDeque,
                MutList[(ReentrantLock, Condition)],
                Condition
            )
    }

    enum TypedDeque {
        case EQI8(MutDeque[Int8])
        case EQI16(MutDeque[Int16])
        case EQI32(MutDeque[Int32])
        case EQI64(MutDeque[Int64])
        case EQF32(MutDeque[Float32])
        case EQF64(MutDeque[Float64])
        case EQBool(MutDeque[Bool])
        case EQChar(MutDeque[Char])
        case EQObj(MutDeque[##java.lang.Object])
    }

    pub def newInt8(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewInt8)

    pub def newInt16(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewInt16)

    pub def newInt32(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewInt32)

    pub def newInt64(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewInt64)

    pub def newFloat32(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewFloat32)

    pub def newFloat64(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewFloat64)

    pub def newBool(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewBool)

    pub def newChar(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewChar)

    pub def newObj(bufferSize: Int): Result[ChannelImpl, String] & Impure =
        bufferCheck(bufferSize, uncheckedNewObj)

    def bufferCheck(bufferSize: Int, continuation: Int -> ChannelImpl & eff): Result[ChannelImpl, String] & eff =
        if (bufferSize < 0) Err("Channel's buffer size must be non-negative.")
        else Ok(continuation(bufferSize))

    pub def uncheckedNewInt8(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQI8(MutDeque.new()))

    pub def uncheckedNewInt16(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQI16(MutDeque.new()))

    pub def uncheckedNewInt32(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQI32(MutDeque.new()))

    pub def uncheckedNewInt64(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQI64(MutDeque.new()))

    pub def uncheckedNewFloat32(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQF32(MutDeque.new()))

    pub def uncheckedNewFloat64(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQF64(MutDeque.new()))

    pub def uncheckedNewBool(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQBool(MutDeque.new()))

    pub def uncheckedNewChar(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQChar(MutDeque.new()))

    pub def uncheckedNewObj(bufferSize: Int): ChannelImpl & Impure =
        newChannelWithQueue(bufferSize, EQObj(MutDeque.new()))

    def newChannelWithQueue(bufferSize: Int, typedDeque: TypedDeque): ChannelImpl & Impure =
        import dev.flix.runtime.GlobalCounter:newId();
        if (bufferSize < 0) inputBug("newChannelWithQueue", "bufferSize < 0") as & Impure
        else ();
        let buffered = bufferSize == 0;
        let reentrantLock = ReentrantLock.new(false);
        Mpmc(
            newId(),
            reentrantLock,
            buffered,
            if (buffered) 1 else bufferSize,
            typedDeque,
            MutList.new(),
            ReentrantLock.newCondition(reentrantLock)
        )

    pub def putInt8(channel: ChannelImpl, element: Int8): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI8(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putInt16(channel: ChannelImpl, element: Int16): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI16(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putInt32(channel: ChannelImpl, element: Int32): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI32(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putInt64(channel: ChannelImpl, element: Int64): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI64(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putFloat32(channel: ChannelImpl, element: Float32): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQF32(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putFloat64(channel: ChannelImpl, element: Float64): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQF64(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putBool(channel: ChannelImpl, element: Bool): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQBool(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putChar(channel: ChannelImpl, element: Char): ChannelImpl & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQChar(deque), _, _) => put(channel, element, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }

    pub def putObj(channel: ChannelImpl, element: a): ChannelImpl & Impure = {
        let Mpmc(_, _, _, _, typedDeque, _, _) = channel;
        match typedDeque {
            case EQObj(deque) => put(channel, element as ##java.lang.Object, deque)
            case _ => inputBug("putInt8", "wrong element type")
        }
    }

    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Implements the expression `c <- x`.
    ///
    def put(channel: ChannelImpl, element: a, deque: MutDeque[a]): ChannelImpl & Impure =
        let Mpmc(_, channelLock, buffered, _, _, waitingGetters, waitingSetters) = channel;
        ReentrantLock.lock(channelLock);

        // Block until the channel is not full
        awaitAvailableSpace(channel);

        // Insert the new element
        MutDeque.pushBack(element, deque);

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
        let Mpmc(_, _, _, maxQueueSize, typedDeque, _, waitingSetters) = channel;
        let size = match typedDeque {
            case EQI8(deque) => MutDeque.size(deque)
            case EQI16(deque) => MutDeque.size(deque)
            case EQI32(deque) => MutDeque.size(deque)
            case EQI64(deque) => MutDeque.size(deque)
            case EQF32(deque) => MutDeque.size(deque)
            case EQF64(deque) => MutDeque.size(deque)
            case EQBool(deque) => MutDeque.size(deque)
            case EQChar(deque) => MutDeque.size(deque)
            case EQObj(deque) => MutDeque.size(deque)
        };
        if (size == maxQueueSize) {
            awaitCondition(waitingSetters);
            awaitAvailableSpace(channel)
        } else {
            ()
        }

    pub def getInt8(channel: ChannelImpl): Int8 & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI8(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getInt16(channel: ChannelImpl): Int16 & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI16(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getInt32(channel: ChannelImpl): Int32 & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI32(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getInt64(channel: ChannelImpl): Int64 & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQI64(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getFloat32(channel: ChannelImpl): Float32 & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQF32(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getFloat64(channel: ChannelImpl): Float64 & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQF64(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getBool(channel: ChannelImpl): Bool & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQBool(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getChar(channel: ChannelImpl): Char & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQChar(deque), _, _) => get(channel, deque)
            case _ => inputBug("getInt8", "wrong get call")
        }

    pub def getObj(channel: ChannelImpl): a & Impure =
        match channel {
            case Mpmc(_, _, _, _, EQObj(deque), _, _) => get(channel, deque) as a
            case _ => inputBug("getInt8", "wrong get call")
        }

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Implements to the expression `<- c`.
    ///
    def get(channel: ChannelImpl, deque: MutDeque[a]): a & Impure =
        let Mpmc(_, channelLock, _, _, _, _, waitingSetters) = channel;
        ReentrantLock.lock(channelLock);
        // Pop the next element
        let optionalElement = MutDeque.popFront(deque);
        let element = getHelper(channel, optionalElement, deque);

        // Signal waiting setters that the channel has space
        signalCondition(waitingSetters);

        unlockLock(channelLock);
        element

    ///
    /// Recursive helper function for get, loops and awaits on empty channel.
    /// The channel lock is expected to be held.
    ///
    def getHelper(channel: ChannelImpl, element: Option[a], deque: MutDeque[a]): a & Impure =
        match element {
            case None => {
                let Mpmc(_, channelLock, _, _, _, waitingGetters, _) = channel;
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

                getHelper(channel, optionalElement, deque)
            }
            case Some(e) => e
        }

    def tryGet(channel: ChannelImpl): Option[Boxed] & Impure =
        import java.lang.Object.hashCode();
        import java.lang.Object.toString();

        let valueToObjBox = v -> BoxedObject(
            v,
            (v1, v2) -> (hashCode(v1 as ##java.lang.Object) as & Pure) <=> (hashCode(v2 as ##java.lang.Object) as & Pure),
            v1 -> toString(v1 as ##java.lang.Object) as & Pure
        );

        let Mpmc(_, channelLock, _, _, typedDeque, _, waitingSetters) = channel;
        ReentrantLock.lock(channelLock);

        // Try to get an element from the channel
        let element = match typedDeque {
            case EQI8(deque) => Option.map(BoxedInt8, MutDeque.popFront(deque))
            case EQI16(deque) => Option.map(BoxedInt16, MutDeque.popFront(deque))
            case EQI32(deque) => Option.map(BoxedInt32, MutDeque.popFront(deque))
            case EQI64(deque) => Option.map(BoxedInt64, MutDeque.popFront(deque))
            case EQF32(deque) => Option.map(BoxedFloat32, MutDeque.popFront(deque))
            case EQF64(deque) => Option.map(BoxedFloat64, MutDeque.popFront(deque))
            case EQBool(deque) => Option.map(BoxedBool, MutDeque.popFront(deque))
            case EQChar(deque) => Option.map(BoxedChar, MutDeque.popFront(deque))
            case EQObj(deque) => Option.map(valueToObjBox, MutDeque.popFront(deque))
        };

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

    def threadInterrupted(): Bool & Impure =
        import java.lang.Thread:interrupted();
        interrupted()

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
        if (index < 0) {
            // Passed `index` less than 0
            internalBug("firstAvailableElement", "index < 0")
        } else if (index >= Array.length(channels)) {
            None
        } else {
            match tryGet(channels[index]) {
                case None =>
                    // try the next channel
                    firstAvailableElement(index + 1, channels)
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
