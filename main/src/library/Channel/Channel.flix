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

    // todo: remove this
    def exit(status: Int32): () & Impure =
        import java.lang.Runtime:getRuntime();
        let runtime = getRuntime();
        import java.lang.Runtime.exit(Int32);
        runtime.exit(status)

    /// The tuple corresponds to (id, channelLock, bufferSize, unbuffered, elementQueue, waitingGetters, waitingSetters)
    pub opaque type Channel[a] = (
        Int64,
        ReentrantLock,
        Int,
        Bool,
        MutList[a],
        MutSet[LockConditionPair],
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

    pub def getWaitingGetters(c: Channel[a]): MutSet[LockConditionPair] = match c {
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

    instance Eq[Channel] {
        pub def eq(x: Channel[a], y: Channel[b]): Bool =
            channelId(x) == channelId(y)
    }

    def channelId(channel: Channel[a]): Int =
        let Channel(c) = channel;
        c.id

    type alias Lock = ##java.util.concurrent.locks.Lock

    type alias LockConditionPair = (Lock, Condition) /// TODO: change to (ReentrantLock, Condition)

    pub def getLock(pair: LockConditionPair): Lock = match pair {
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

    type alias ReentrantLock = ##java.util.concurrent.locks.ReentrantLock

    def newReentrantLock(): ReentrantLock & Impure =
        import new java.util.concurrent.locks.ReentrantLock() as javaNewReentrantLock;
        javaNewReentrantLock()

    type alias Condition = ##java.util.concurrent.locks.Condition

    def newCondition(reentrantLock: ReentrantLock): Condition & Impure =
        import java.util.concurrent.locks.ReentrantLock.newCondition() as javaNewCondition;
        javaNewCondition(reentrantLock)

    def newId(): Int64 & Impure =
        import library.Channel.GlobalCounter:newId() as javaNewId;
        javaNewId()

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
            MutSet.empty(),
            newCondition(reentrantLock)
        )
    ///
    /// Receives an element from the channel `c`.
    ///
    /// Equivalent to the expression `<- c`.
    ///
    pub def get(c: Channel[a]): a & Impure = <- c

    pub def loopOnFullChannel(c: Channel[a], bufferSize: Int): () & Impure = try {
        import java.util.concurrent.locks.Condition.await();
        // If channel not full, return
        if (MutList.length(getElementQueue(c)) < bufferSize) ()
        // Else, wait
        else {
        getWaitingSetters(c).await();
        loopOnFullChannel(c, bufferSize)
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
        import java.util.concurrent.locks.ReentrantLock.lock() as r_lock;
        import java.util.concurrent.locks.ReentrantLock.unlock() as r_unlock;
        import java.util.concurrent.locks.Lock.lock();
        import java.util.concurrent.locks.Lock.unlock();
        import java.util.concurrent.locks.Condition.signalAll();
        import java.util.concurrent.locks.Condition.await();

        getChannelLock(c).r_lock();

        try {
            // Check if the channel is full
            loopOnFullChannel(c, getBufferSize(c));

            // There was space to put another element in the channel
            MutList.push!(x, getElementQueue(c));

            // Signal waitingGetters that there is an element available
            MutSet.foreach(
                (pair) -> {
                    let conditionLock = getLock(pair);
                    let condition = getCondition(pair);
                    try {
                        conditionLock.lock();
                        condition.signalAll()
                    } catch {
                       case _: ##java.lang.Exception => conditionLock.unlock()
                    };
                        conditionLock.unlock()
                },
                getWaitingGetters(c));

            // Clear waitingGetters.
            // If a waitingGetter does not receive an element, it can add itself again
            MutSet.clear!(getWaitingGetters(c));

            // If the channel is unbuffered, wait for the element to be handed off before continuing
            if (getUnbuffered(c)) getWaitingSetters(c).await()
            else ()
        } catch {
            case _: ##java.lang.Exception => getChannelLock(c).r_unlock()
        };
        getChannelLock(c).r_unlock();
        c


def cmp(a: Channele[a], b: Channele[a]): Int32=
    let a_id = getId(a);
    let b_id = getId(b);
    if (a_id == b_id) (0)
    else if (a_id > b_id) (1)
    else (-1)

def sortChannels(channels: Array[Channele[a]]): Array[Channele[a]] & Impure=
    Array.sortWith(cmp, channels)

def lock(chan: Channele[a]): Unit & Impure=
    let lock = getChannelLock(chan);
    java_lock(lock);
    ()

def unlock(chan: Channele[a]): Unit & Impure= try {
    let lock = getChannelLock(chan);
    java_unlock(lock);
    ()
} catch {
    case _: ##java.lang.Exception => ()
}

def lockAllChannels(channels: Array[Channele[a]]): Unit & Impure=
    Array.foreach(lock,channels);
    ()
def unlockAllChannels(channels: Array[Channele[a]]): Unit & Impure=
    Array.foreach(unlock,channels);
    ()


def tryGet(chan: Channele[a]): a & Impure=
    lock(chan);
    let queue = getElementQueue(chan); ///TODO move inside try/catch and return null in catch
    /// let len = MutList.length(queue)
    let element = MutList.get(0,queue); 
    try {
        match element {
            case e => {
                unlock(chan);
                e
            }
            case _ => {
                let ws = getWaitingSetters(chan);
                java_signalAll(ws);
                unlock(chan);
                element
            }
        }
    } catch {
        case _: ##java.lang.Exception => { unlock(chan); element }
    }


/// def get_internal_loop(chan: Channele[a], e: Option[a]): Unit & Impure=
///     let queue = getElementQueue(chan);
///     /// let e = MutList.get(0,queue);
///     match e {
///             case _ => { //not null??
///                 ()
///             }
///             case None => { ///null
///                 let conditionLock = newReentrantLock();
///                 java_lock(conditionLock);
///                 try {
///                     let condition = newCondition(conditionLock);
///                     addGetter(chan, conditionLock, condition);
///                     try {
///                         let channelLock = getChannelLock(chan);
///                         java_unlock(channelLock);
///                         java_await(condition)
///                     } catch {
///                         case _: ##java.lang.Exception => { lock(chan) }
///                     }
///                    /// e = MutList.get(0,queue);
///                 } catch {
///                     case _: ##java.lang.Exception => { java_unlock(conditionLock); 
///                                                        get_internal_loop(chan,MutList.get(0,queue)) 
///                     }
///                 }
///             }
///         }

    
    

/// def get(chan: Channele[a]): a & Impure=
///     lock(chan);
///     let queue = getElementQueue(chan);
///     let e = MutList.get(0,queue);
///     try {
///         match e {
///             case element => {
///                 element
///             }
///             case _ => { ///null
///                 get_internal_loop(chan,e);
///                 java_signalAll(getWaitingSetters(chan));
///                 unlock(chan);
///                 e
///             }
///         }
///     } catch {
///         case _: ##java.lang.Exception => e
///   }
}
