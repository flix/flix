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

// TODO : remove this
def printf[a, e](toString: a -> String & e, str: String, val: a): Unit & Impure =
    let stringBuilder = StringBuilder.new();
    let convertedStr = toString(val);
    StringBuilder.appendString!(stringBuilder, str);
    StringBuilder.appendString!(stringBuilder, convertedStr);
    let fullStr = StringBuilder.toString(stringBuilder);
    Console.printLine(fullStr)


namespace ChannelImpl {

    // todo: remove this
    def exit(status: Int32): () & Impure =
        import java.lang.Runtime:getRuntime();
        let runtime = getRuntime();
        import java.lang.Runtime.exit(Int32);
        runtime.exit(status)

    /**
    * The tuple corresponds to (id, channelLock, bufferSize, unbuffered, elementQueue, waitingGetters, waitingSetters)
    *   - id
    *   - channelLock is the channelLock of this channel.
    *   - bufferSize is the size of a channel. If you try to put an
    *   element in a channel that's full, you wait until there's space.
    *   - a flag for whether the channel is unbuffered or not
    *   - elementQueue is the elementQueue of elements in the list.
    *   - waitingGetters is a set of conditions that is waiting for get.
    *   This set is cleared after each new element.
    *   - waitingSetters is a condition that can notify threads of
    *   available space in the elementQueue.
    */
    pub opaque type ChannelImpl[a] = (
        Int64,
        ReentrantLock,
        Int,
        Bool,
        MutList[a],
        MutList[LockConditionPair],
        Condition
    )


    // channel, index
    pub opaque type ChannelIndexPair[a] = (
        ChannelImpl[a],
        Int
        )

    pub def newChannelIndexPair(channel: ChannelImpl[a], index: Int): ChannelIndexPair[a] =
        ChannelIndexPair(
            channel,
            index
        )

    pub def getChannel(cip: ChannelIndexPair[a]): ChannelImpl[a] = match cip {
            case ChannelIndexPair(c, _)   => c
        }

    pub def getIndex(cip: ChannelIndexPair[a]): Int = match cip {
            case ChannelIndexPair(_, ind)   => ind
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


    pub def getId(c: ChannelImpl[a]): Int64 = match c {
            case ChannelImpl(id, _, _, _, _, _, _)   => id
        }

    pub def getChannelLock(c: ChannelImpl[a]): ReentrantLock = match c {
            case ChannelImpl(_, channelLock, _, _, _, _, _)   => channelLock
        }

    pub def getBufferSize(c: ChannelImpl[a]): Int = match c {
            case ChannelImpl(_, _, bufferSize, _, _, _, _)   => bufferSize
        }

    pub def getUnbuffered(c: ChannelImpl[a]): Bool = match c {
            case ChannelImpl(_, _, _, unbuffered, _, _, _)   => unbuffered
        }

    pub def getElementQueue(c: ChannelImpl[a]): MutList[a] = match c {
            case ChannelImpl(_, _, _, _, elementQueue, _, _)   => elementQueue
        }

    // TODO : make it a MutList or MutSet??
    pub def getWaitingGetters(c: ChannelImpl[a]): MutList[LockConditionPair] = match c {
            case ChannelImpl(_, _, _, _, _, waitingGetters, _)   => waitingGetters
        }

    pub def getWaitingSetters(c: ChannelImpl[a]): Condition = match c {
            case ChannelImpl(_, _, _, _, _, _, waitingSetters)   => waitingSetters
        }


    /*
    instance Order[Channel] {
        pub def compare(x: Channel, y: Channel): Comparison =
            compare(x.id, y.id)
    }
    */

    /// instance Eq[Channel] {
    ///     pub def eq(x: Channel[a], y: Channel[b]): Bool =
    ///         channelId(x) == channelId(y)
    /// }

    /// def channelId(channel: Channel[a]): Int =
    ///     let Channel(c) = channel;
    ///     getId(c)

    type alias ReentrantLock = ##java.util.concurrent.locks.ReentrantLock

    /// type alias Lock = ##java.util.concurrent.locks.ReentrantLock

    type alias LockConditionPair = (ReentrantLock, Condition)

    pub def getLock(pair: LockConditionPair): ReentrantLock = match pair {
            case (lock, _)   => lock
        }

    pub def getCondition(pair: LockConditionPair): Condition = match pair {
            case (_, cond)   => cond
        }

    def java_signalAll(cond: Condition): Unit & Impure =
        import java.util.concurrent.locks.Condition.signalAll() as signalAll;
        signalAll(cond);
        ()

    def java_lock(reentrantLock: ReentrantLock): Unit & Impure =
        import java.util.concurrent.locks.ReentrantLock.lock() as lock;
        lock(reentrantLock);
        ()

    def java_unlock(reentrantLock: ReentrantLock): Unit & Impure =
        import java.util.concurrent.locks.ReentrantLock.unlock() as unlock;
        unlock(reentrantLock);
        ()

    def java_await(condition: Condition): Unit & Impure =
        import java.util.concurrent.locks.Condition.await();
        condition.await()

    def newReentrantLock(): ReentrantLock & Impure =
        import new java.util.concurrent.locks.ReentrantLock() as javaNewReentrantLock;
        javaNewReentrantLock()

    type alias Condition = ##java.util.concurrent.locks.Condition

    def newCondition(reentrantLock: ReentrantLock): Condition & Impure =
        import java.util.concurrent.locks.ReentrantLock.newCondition() as javaNewCondition;
        javaNewCondition(reentrantLock)

    /// def newId(): Int64 & Impure =
    ///     import library.Channel.GlobalCounter:newId() as javaNewId;
    ///     javaNewId()

    ///
    /// Returns a fresh unbuffered channel.
    ///
    /// An unbuffered channel has an internal capacity of `0`.
    ///
    pub def new(): ChannelImpl[a] & Impure = newWithCapacity(0)

    ///
    /// Returns a fresh buffered channel with internal capacity `c`.
    ///
    pub def newWithCapacity(bufferSize: Int): ChannelImpl[a] & Impure =

///   TODO  This does not do what you think it does :)
/// Here is what you want:
/// let unbuffered = ref false;
/// ...
/// unbuffered := true

        let unbuffered = false;
        if (bufferSize < 0) {
            // TODO bug!("foo")
            Console.printLine("Channel bufferSize must be positive");
            exit(1)
        } else if (bufferSize == 0) {
            ref bufferSize := 1;
            ref unbuffered := true
        }
        else
            ref unbuffered := false;

        let mutList = MutList.new();
        MutList.reserve!(bufferSize, mutList);

        let reentrantLock = newReentrantLock();

        ChannelImpl(
            0i64,
            reentrantLock,
            bufferSize,
            unbuffered,
            mutList,
            MutList.new(), // MutSet.empty()
            newCondition(reentrantLock)
        )
    ///
    /// Receives an element from the channel `c`.
    ///
    /// Equivalent to the expression `<- c`.
    ///
    /// pub def get(c: Channel[a]): a & Impure = <- c

    pub def put_waitOnFullChannel(c: ChannelImpl[a], bufferSize: Int): () & Impure =
        // If channel not full, return
        if (MutList.length(getElementQueue(c)) < bufferSize) ()
        // Else, wait
        else {
        java_await(getWaitingSetters(c));
        put_waitOnFullChannel(c, bufferSize)
        }


    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Equivalent to the expression `c <- x`.
    ///
    pub def put(c: ChannelImpl[a], x: a): ChannelImpl[a] & Impure =
        lock(c);

        // Check if the channel is full
        put_waitOnFullChannel(c, getBufferSize(c));

        // There was space to put another element in the channel
        MutList.push!(x, getElementQueue(c));

        // Signal waitingGetters that there is an element available
        MutList.foreach(
            (pair) -> {
                let conditionLock = getLock(pair);
                java_lock(conditionLock);
                signalAll(pair);
                java_unlock(conditionLock)
            },
            getWaitingGetters(c));

        // Clear waitingGetters.
        // If a waitingGetter does not receive an element, it can add itself again
        MutList.clear!(getWaitingGetters(c));

        // If the channel is unbuffered, wait for the element to be handed off before continuing
        if (getUnbuffered(c)) java_await(getWaitingSetters(c))
        else ();

        unlock(c);
        c

    pub def threadInterrupted(): Bool & Impure =
        /// import java.lang.Thread:currentThread();
        /// let thread = currentThread();
        import java.lang.Thread:interrupted();
        let isInterrupted = interrupted();
        if (isInterrupted) true
        else false


    pub def select_helper(channels: Array[ChannelImpl[a]], sortedChannels: Array[ChannelImpl[a]], selectLock: ReentrantLock, condition: Condition, hasDefault: Bool): Option[(Int, a, Bool)] & Impure =
        // MutList used to persist the Select Choice from the inner scope
        let selectChoices = MutList.new();

        if (threadInterrupted()) {Console.printLine("Thread is interrupted"); None}
        else {
            // Lock all channels in sorted order
            lockAllChannels(sortedChannels);

            // Lock the select lock after the channels
            java_lock(selectLock);

            // Find channels with waiting elements in a random order to prevent backpressure.
            // Build list mapping a channel to it's branchNumber (the index of the 'channels' array)
            let channelIndexPairs = MutList.new();
            Array.foreachWithIndex(
                (c, i) -> MutList.push!(newChannelIndexPair(c, i), channelIndexPairs),
                channels
                );


            // TODO : Randomize the order channels are looked at.
            // This prevents backpressure from building up on one channel.
            /// Collections.shuffle(channelIndexPairs, ThreadLocalRandom.current());

            // Find channels with waiting elements
            MutList.foreachWithIndex(
                (cip, _) -> {
                    let e_opt = tryGet(getChannel(cip));

                    match e_opt {
                        case None => ()
                        case Some(element) => {
                            // There is a waiting element in this channel.
                            // Return the element and the branchNumber of this channel

                            // If an element found, add a new selectChoice to the MutList in order to later return
                            let selectChoice = Some((getIndex(cip), element, false));
                            MutList.push!(selectChoice, selectChoices)
                        }
                    }
                },
                channelIndexPairs
                );


            // Element found
            if (!MutList.isEmpty(selectChoices)) {
                Console.printLine("Selected an element");

                MutList.get(0, selectChoices)}
            // No element was found but if there is a default case, choose this
            else if (hasDefault) {
                Console.printLine("Default selection");
                // TODO what exactly to return?
                None
            }
            // No element was found.
            else {
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
                java_await(condition);

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


    def cmp(a: ChannelImpl[a], b: ChannelImpl[a]): Int32=
        let a_id = getId(a);
        let b_id = getId(b);
        if (a_id == b_id) (0)
        else if (a_id > b_id) (1)
        else (-1)

    def sortChannels(channels: Array[ChannelImpl[a]]): Array[ChannelImpl[a]] & Impure=
        Array.sortWith(cmp, channels)

    def lock(chan: ChannelImpl[a]): Unit & Impure=
        let lock = getChannelLock(chan);
        java_lock(lock);
        ()

    def unlock(chan: ChannelImpl[a]): Unit & Impure=
        let lock = getChannelLock(chan);
        java_unlock(lock);
        ()


    def signalAll(pair: LockConditionPair): Unit & Impure=
        let condition = getCondition(pair);
        Console.printLine("java_signalAll called");
        java_signalAll(condition)

    def lockAllChannels(channels: Array[ChannelImpl[a]]): Unit & Impure=
        Array.foreach(lock,channels);
        ()

    def unlockAllChannels(channels: Array[ChannelImpl[a]]): Unit & Impure=
        Array.foreach(unlock,channels);
        ()


    /*
    * Recursive helper function for get, loops and awaits on empty channel
    */
    def get_waitOnEmptyChannel(chan: ChannelImpl[a], e: Option[a]): Option[a] & Impure=
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
                java_await(condition);
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



    pub def get(chan: ChannelImpl[a]): a & Impure=
        lock(chan);
        let queue = getElementQueue(chan);

        let e_opt = MutList.pop!(queue);
        get_waitOnEmptyChannel(chan, e_opt);

        // Unwrap the Optional type element
        let e_mutList = List.toMut(Option.toList(e_opt));
        let e = MutList.get(0, e_mutList);

        // Signal waiting setters that the channel has space
        let waitingSetters = getWaitingSetters(chan);
        java_signalAll(waitingSetters);
        unlock(chan);

        // Return the element from the channel
        e
    // TODO : test putting on full channel



    def tryGet(chan: ChannelImpl[a]): Option[a] & Impure=
        lock(chan);
        let queue = getElementQueue(chan);
        let element = MutList.pop!(queue);

        match element {
            case None => ()
            case _ => {
                let ws = getWaitingSetters(chan);
                java_signalAll(ws)
            }
        };

        unlock(chan);
        element


    /**
    * Adds the given condition to the list of conditions waiting to
    * retrieve elements from the elementQueue.
    *
    * @param condition the condition to add
    */
    pub def addGetter(channel: ChannelImpl[a], conditionLock: ReentrantLock, condition: Condition): Unit & Impure=
        lock(channel);
        let pair = (conditionLock, condition);
        // Add LockConditionPair to the channel
        MutList.push!(pair, getWaitingGetters(channel));
        unlock(channel)
}
