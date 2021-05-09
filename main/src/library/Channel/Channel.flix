/*
 * Copyright 2019 Magnus Madsen
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

namespace Channel {

    type alias ReentrantLock = ##java.util.concurrent.locks.ReentrantLock

    def newReentrantLock(): ReentrantLock & Impure =
        import new java.util.concurrent.locks.ReentrantLock() as javaNewReentrantLock;
        javaNewReentrantLock()

    def newCondition(reentrantLock: ReentrantLock): Condition & Impure =
        import java.util.concurrent.locks.ReentrantLock.newCondition() as javaNewCondition;
        javaNewCondition(reentrantLock)

    def java_lock(reentrantLock: ReentrantLock): Unit & Impure =
        import java.util.concurrent.locks.ReentrantLock.lock() as javaLock;
        javaLock(reentrantLock)

    def java_unlock(reentrantLock: ReentrantLock): Unit & Impure =
        import java.util.concurrent.locks.ReentrantLock.unlock() as javaUnlock;
        javaUnlock(reentrantLock)

    type alias Condition = ##java.util.concurrent.locks.Condition

    def signalAll(cond: Condition): Unit & Impure =
        import java.util.concurrent.locks.Condition.signalAll() as javaSignalAll;
        javaSignalAll(cond)

    def await(condition: Condition): Unit & Impure =
        import java.util.concurrent.locks.Condition.await() as javaAwait;
        javaAwait(condition)

    /**
    * The tuple corresponds to (id, channelLock, bufferSize, unbuffered, elementQueue, waitingGetters, waitingSetters)
    *   - id
    *   - channelLock is the channelLock of this channel.
    *   - bufferSize is the size of a channel. If you try to put an element in a channel that's full, you wait until there's space.
    *   - a flag for whether the channel is unbuffered or not
    *   - elementQueue is the elementQueue of elements in the list.
    *   - waitingGetters is a set of conditions that is waiting for get. This set is cleared after each new element.
    *   - waitingSetters is a condition that can notify threads of available space in the elementQueue.
    */
    pub enum ChannelImpl[a] {
        case Mpmc(
                Int64,
                ReentrantLock,
                Int,
                Bool,
                MutList[a],
                MutList[(ReentrantLock, Condition)],
                Condition
            )
    }

    /// @Override
    /// pub def equals(o: a): Bool & Impure =
    ///     if (this == o) return true;
    ///     if (o == null || getClass() != o.getClass()) return false;
    ///     ChannelIndexPair that = (ChannelIndexPair) o;
    ///     return index == that.index && Objects.equals(channel, that.channel);
    /// }

    /// @Override
    /// pub def hashCode(cip: ChannelIndexPair): Int =
    ///     import java.util.Objects.hash as hash;
    ///     hash(getChannel(cip), getIndex(cip))

    /// instance Order[ChannelImpl[a]] {
    ///     pub def compare(x: Channel[a], y: Channel[a]): Comparison =
    ///         compare(x.id, y.id)
    /// }

    /// instance Eq[ChannelImpl[a]] {
    ///     pub def eq(x: Channel[a], y: Channel[b]): Bool =
    ///         channelId(x) == channelId(y)
    /// }

    ///
    /// Returns a fresh buffered channel with internal capacity `c`.
    ///
    pub def newWithCapacity(bufferSize: Int): ChannelImpl[a] & Impure =
        let (bufSize, unbuffered) =
            if (bufferSize < 0)
                bug!("Channel's buffer size must be non-negative.")
            else if (bufferSize == 0)
                (1, true)
            else
                (bufferSize, false);

        let mutList = MutList.new();
        MutList.reserve!(bufferSize, mutList);

        let reentrantLock = newReentrantLock();

        Mpmc(
            newId(),
            reentrantLock,
            bufSize,
            unbuffered,
            mutList,
            MutList.new(), // MutSet.empty()
            newCondition(reentrantLock)
        )

    pub def waitOnFullChannel(c: ChannelImpl[a], bufferSize: Int): () & Impure =
        if (MutList.length(getElementQueue(c)) < bufferSize) 
            // If channel not full, return
            ()
        else {
            // Else, wait
            await(getWaitingSetters(c));
            waitOnFullChannel(c, bufferSize)
        }


    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Equivalent to the expression `c <- x`.
    ///
    pub def put(c: ChannelImpl[a], x: a): ChannelImpl[a] & Impure =
        lock(c);

        // Check if the channel is full
        waitOnFullChannel(c, getBufferSize(c));

        // There was space to put another element in the channel
        MutList.push!(x, getElementQueue(c));

        // Signal waitingGetters that there is an element available
        MutList.foreach(
            (lockConditionPair) -> {
                let (conditionLock, condition) = lockConditionPair;
                java_lock(conditionLock);
                signalAll(condition);
                java_unlock(conditionLock)
            },
            getWaitingGetters(c));

        // Clear waitingGetters.
        // If a waitingGetter does not receive an element, it can add itself again
        MutList.clear!(getWaitingGetters(c));

        // If the channel is unbuffered, wait for the element to be handed off before continuing
        if (getUnbuffered(c)) 
            await(getWaitingSetters(c))
        else 
            ();

        unlock(c);
        c

    /*
    * Recursive helper function for get, loops and awaits on empty channel
    */
    def get_waitOnEmptyChannel(chan: ChannelImpl[a], e: Option[a]): Option[a] & Impure =
        let queue = getElementQueue(chan);

        match e {
            case None => { // Null
                Console.printLine("Empty channel");

                // Create a new Lock and Condition
                let conditionLock = newReentrantLock();
                java_lock(conditionLock);
                let condition = newCondition(conditionLock);

                // Create a new LockConditionPair
                let pair = (conditionLock, condition);
                let getters = getWaitingGetters(chan);

                // Add LockConditionPair to the channel
                MutList.push!(pair, getters);

                // Temporarily unlock the channel while waiting. This is necessary as the Condition comes from a different Lock.
                unlock(chan);
                await(condition);
                lock(chan);

                // TODO :  will always pop LIFO, should pop randomly?
                // Someone signalled that an element was put in the channel.
                // Try to get the element (which could already be taken by someone else)
                ref e := MutList.pop!(queue);

                java_unlock(conditionLock);
                get_waitOnEmptyChannel(chan, e);

                // Return the element to get
                e
            }
            case _ => { //Not Null
                // Return the element to get
                e
            }
        }

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Equivalent to the expression `<- c`.
    ///
    /// pub def get(c: Channel[a]): a & Impure = <- c
    pub def get(chan: ChannelImpl[a]): a & Impure =
        lock(chan);
        let queue = getElementQueue(chan);

        let e_opt = MutList.pop!(queue);
        get_waitOnEmptyChannel(chan, e_opt);

        // Unwrap the Optional type element
        let e_mutList = List.toMut(Option.toList(e_opt));
        let e = MutList.get(0, e_mutList);

        // Signal waiting setters that the channel has space
        let waitingSetters = getWaitingSetters(chan);
        signalAll(waitingSetters);
        unlock(chan);

        // Return the element from the channel
        e
    // TODO : test putting on full channel

    def tryGet(chan: ChannelImpl[a]): Option[a] & Impure =
        lock(chan);
        let queue = getElementQueue(chan);
        let element = MutList.pop!(queue);

        match element {
            case None => ()
            case _ => {
                let ws = getWaitingSetters(chan);
                signalAll(ws)
            }
        };

        unlock(chan);
        element


    pub def select_helper(channels: Array[ChannelImpl[a]], sortedChannels: Array[ChannelImpl[a]], selectLock: ReentrantLock, condition: Condition, hasDefault: Bool): Option[(Int, a, Bool)] & Impure =
        // MutList used to persist the Select Choice from the inner scope
        let selectChoices = MutList.new();

        if (threadInterrupted()) {
            Console.printLine("Thread is interrupted"); 
            None
        } else {
            // Lock all channels in sorted order
            lockAllChannels(sortedChannels);

            // Lock the select lock after the channels
            java_lock(selectLock);

            // Find channels with waiting elements in a random order to prevent backpressure.
            // Build list mapping a channel to it's branchNumber (the index of the 'channels' array)
            let channelIndexPairs = MutList.new();
            Array.foreachWithIndex(
                (c, i) -> MutList.push!((c, i), channelIndexPairs),
                channels
                );


            // TODO : Randomize the order channels are looked at.
            // This prevents backpressure from building up on one channel.
            /// Collections.shuffle(channelIndexPairs, ThreadLocalRandom.current());

            // Find channels with waiting elements
            MutList.foreachWithIndex(
                (cip, _) -> {
                    let e_opt = tryGet(fst(cip));

                    match e_opt {
                        case None => ()
                        case Some(element) => {
                            // There is a waiting element in this channel.
                            // Return the element and the branchNumber of this channel

                            // If an element found, add a new selectChoice to the MutList in order to later return
                            let selectChoice = Some((snd(cip), element, false));
                            MutList.push!(selectChoice, selectChoices)
                        }
                    }
                },
                channelIndexPairs
                );


            // Element found
            if (not MutList.isEmpty(selectChoices)) {
                Console.printLine("Selected an element");

                MutList.get(0, selectChoices)
            }
            // No element was found but if there is a default case, choose this
            else if (hasDefault) {
                Console.printLine("Default selection");
                // TODO what exactly to return?
                None
            } else { // No element was found.
                Console.printLine("Nothing selected");
                // Add our condition to all channels to get notified when a new element is added
                Array.foreach(
                    (c) -> {
                        addGetter(c, selectLock, condition)
                    },
                    channels);

                // Unlock all channels in sorted order, so other threads may input elements
                unlockAllChannels(sortedChannels);

                // Wait for an element to be added to any of the channels
                await(condition);

                // Unlock the selectLock, which is relevant when a different thread wants to put
                // an element into a channel that was not selected from the select.
                // This other channel will then signal the condition from selectLock (in the put method),
                // so it needs the lock.
                java_unlock(selectLock);

                select_helper(channels, sortedChannels, selectLock, condition, hasDefault)
            }
        }

  /**
   * Given a array of channels, returns the first channel that has an element
   * and return the index of that channel and the retrieved element in a
   * SelectChoice object.
   *
   * @param channels the channels to select on
   * @return the channel index of the channel with an element and the element
   */
   // TODO : resolve the function name issue
    pub def selectImpl(channels: Array[ChannelImpl[a]], hasDefault: Bool): Option[(Int, a, Bool)] & Impure =
        // Create new Condition and channelLock the current thread
        let selectLock = newReentrantLock();
        let condition = newCondition(selectLock);

        // Sort channels to avoid deadlock when locking
        let sortedChannels = sortChannels(channels);
        select_helper(channels, sortedChannels, selectLock, condition, hasDefault)

    pub def getId(c: ChannelImpl[a]): Int64 = match c {
            case Mpmc(id, _, _, _, _, _, _)   => id
        }

    pub def getChannelLock(c: ChannelImpl[a]): ReentrantLock = match c {
            case Mpmc(_, channelLock, _, _, _, _, _)   => channelLock
        }

    pub def getBufferSize(c: ChannelImpl[a]): Int = match c {
            case Mpmc(_, _, bufferSize, _, _, _, _)   => bufferSize
        }

    pub def getUnbuffered(c: ChannelImpl[a]): Bool = match c {
            case Mpmc(_, _, _, unbuffered, _, _, _)   => unbuffered
        }

    pub def getElementQueue(c: ChannelImpl[a]): MutList[a] = match c {
            case Mpmc(_, _, _, _, elementQueue, _, _)   => elementQueue
        }

    // TODO : make it a MutList or MutSet??
    pub def getWaitingGetters(c: ChannelImpl[a]): MutList[(ReentrantLock, Condition)] = match c {
            case Mpmc(_, _, _, _, _, waitingGetters, _)   => waitingGetters
        }

    pub def getWaitingSetters(c: ChannelImpl[a]): Condition = match c {
            case Mpmc(_, _, _, _, _, _, waitingSetters)   => waitingSetters
        }

    def cmp(a: ChannelImpl[a], b: ChannelImpl[a]): Int32 =
        let a_id = getId(a);
        let b_id = getId(b);
        if (a_id == b_id) 
            (0)
        else if (a_id > b_id) 
            (1)
        else 
            (-1)

    def sortChannels(channels: Array[ChannelImpl[a]]): Array[ChannelImpl[a]] & Impure =
        Array.sortWith(cmp, channels)

    def lock(chan: ChannelImpl[a]): Unit & Impure =
        let lock = getChannelLock(chan);
        java_lock(lock)

    def unlock(chan: ChannelImpl[a]): Unit & Impure =
        let lock = getChannelLock(chan);
        java_unlock(lock)

    def lockAllChannels(channels: Array[ChannelImpl[a]]): Unit & Impure =
        Array.foreach(lock, channels)

    def unlockAllChannels(channels: Array[ChannelImpl[a]]): Unit & Impure =
        Array.foreach(unlock, channels)

    /**
    * Adds the given condition to the list of conditions waiting to
    * retrieve elements from the elementQueue.
    *
    * @param condition the condition to add
    */
    pub def addGetter(channel: ChannelImpl[a], conditionLock: ReentrantLock, condition: Condition): Unit & Impure =
        lock(channel);
        let pair = (conditionLock, condition);

        // Add LockConditionPair to the channel
        MutList.push!(pair, getWaitingGetters(channel));
        unlock(channel)

    def newId(): Int64 & Impure =
        import library.Channel.GlobalCounter:newId() as javaNewId;
        javaNewId()

    pub def threadInterrupted(): Bool & Impure =
        import java.lang.Thread:interrupted();
        let isInterrupted = interrupted();
        if (isInterrupted)
            true
        else
            false

}
