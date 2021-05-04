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

namespace Channel {

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
    pub opaque type Channel[a] = (
        Int64,
        ReentrantLock,
        Int,
        Bool,
        MutList[a],
        MutList[LockConditionPair],
        Condition
    )

    pub def getId(c: Channel[a]): Int64 = match c {
            case Channel(id, _, _, _, _, _, _)   => id
        }

    pub def getChannelLock(c: Channel[a]): ReentrantLock = match c {
            case Channel(_, channelLock, _, _, _, _, _)   => channelLock
        }

    pub def getBufferSize(c: Channel[a]): Int = match c {
            case Channel(_, _, bufferSize, _, _, _, _)   => bufferSize
        }

    pub def getUnbuffered(c: Channel[a]): Bool = match c {
            case Channel(_, _, _, unbuffered, _, _, _)   => unbuffered
        }

    pub def getElementQueue(c: Channel[a]): MutList[a] = match c {
            case Channel(_, _, _, _, elementQueue, _, _)   => elementQueue
        }

    // TODO : make it a MutList or MutSet??
    pub def getWaitingGetters(c: Channel[a]): MutList[LockConditionPair] = match c {
            case Channel(_, _, _, _, _, waitingGetters, _)   => waitingGetters
        }

    pub def getWaitingSetters(c: Channel[a]): Condition = match c {
            case Channel(_, _, _, _, _, _, waitingSetters)   => waitingSetters
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
    pub def new(): Channel[a] & Impure = newWithCapacity(0)

    ///
    /// Returns a fresh buffered channel with internal capacity `c`.
    ///
    pub def newWithCapacity(bufferSize: Int): Channel[a] & Impure =
        let unbuffered = false;
        if (bufferSize < 0) {
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

        Channel(
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

    pub def put_waitOnFullChannel(c: Channel[a], bufferSize: Int): () & Impure = try {
        import java.util.concurrent.locks.Condition.await();
        // If channel not full, return
        if (MutList.length(getElementQueue(c)) < bufferSize) ()
        // Else, wait
        else {
        getWaitingSetters(c).await();
        put_waitOnFullChannel(c, bufferSize)
        }
    } catch {
        case _: ##java.lang.Exception => ()
    }


    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Equivalent to the expression `c <- x`.
    ///
    pub def put(c: Channel[a], x: a): Channel[a] & Impure =
        /// import java.util.concurrent.locks.ReentrantLock.lock() as lock2;
        /// import java.util.concurrent.locks.ReentrantLock.unlock() as unlock2;
        /// import java.util.concurrent.locks.Condition.signalAll();
        import java.util.concurrent.locks.Condition.await();

        lock(c);

        // Check if the channel is full
        put_waitOnFullChannel(c, getBufferSize(c));

        // There was space to put another element in the channel
        MutList.push!(x, getElementQueue(c));

        // Signal waitingGetters that there is an element available
        MutList.foreach(
            (pair) -> {
                lockConditionLock(pair);
                signalAll(pair);
                unlockConditionLock(pair)
            },
            getWaitingGetters(c));

        // Clear waitingGetters.
        // If a waitingGetter does not receive an element, it can add itself again
        MutList.clear!(getWaitingGetters(c));

        // If the channel is unbuffered, wait for the element to be handed off before continuing
        if (getUnbuffered(c)) getWaitingSetters(c).await()
        else ();

        unlock(c);
        c


    /// def cmp(a: Channel[a], b: Channel[a]): Int32=
    ///     let a_id = getId(a);
    ///     let b_id = getId(b);
    ///     if (a_id == b_id) (0)
    ///     else if (a_id > b_id) (1)
    ///     else (-1)

    /// def sortChannels(channels: Array[Channel[a]]): Array[Channel[a]] & Impure=
    ///     Array.sortWith(cmp, channels)

    def lock(chan: Channel[a]): Unit & Impure=
        let lock = getChannelLock(chan);
        java_lock(lock);
        ()

    def unlock(chan: Channel[a]): Unit & Impure=
        let lock = getChannelLock(chan);
        java_unlock(lock);
        ()

    def lockConditionLock(pair: LockConditionPair): Unit & Impure=
        let conditionLock = getLock(pair);
        java_lock(conditionLock);
        ()

    def unlockConditionLock(pair: LockConditionPair): Unit & Impure=
        let conditionLock = getLock(pair);
        java_unlock(conditionLock);
        ()

    def signalAll(pair: LockConditionPair): Unit & Impure=
        let condition = getCondition(pair);
        Console.printLine("java_signalAll called");
        java_signalAll(condition)

    /// def lockAllChannels(channels: Array[Channel[a]]): Unit & Impure=
    ///     Array.foreach(lock,channels);
    ///     ()
    /// def unlockAllChannels(channels: Array[Channel[a]]): Unit & Impure=
    ///     Array.foreach(unlock,channels);
    ///     ()


    /// def tryGet(chan: Channel[a]): Option[a] & Impure=
    ///     lock(chan);
    ///     let queue = getElementQueue(chan); ///TODO move inside try/catch and return null in catch
    ///     /// let len = MutList.length(queue)
    ///     let element = MutList.pop!(queue);

    ///     match element {
    ///         case e => {
    ///             let setters = getWaitingSetters(chan);
    ///             java_signalAll(ws)
    ///         }
    ///         case _ => ()
    ///     };
    ///     unlock(chan);
    ///     /// let unwrappedElement = Option.getWithDefault(element, None);
    ///     element

    /*
    * Recursive helper function for get, loops and awaits on empty channel
    */
    def get_waitOnEmptyChannel(chan: Channel[a], e: Option[a]): Option[a] & Impure=
        let queue = getElementQueue(chan);

        match e {
            case None => { // Null
                Console.printLine("null");

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
                Console.printLine("not null");

                // Return the element to get
                e
            }
        }



    pub def get(chan: Channel[a]): a & Impure=
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

    // TODO : remove this
    def waitFiveSec(): Unit & Impure =
        import java.lang.Thread:sleep(Int64);
        let fiveSec = 5000i64;
        sleep(fiveSec)
}