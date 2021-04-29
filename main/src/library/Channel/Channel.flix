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

    type alias LockConditionPair = (Lock, Condition)

    pub def getLock(pair: LockConditionPair): Lock = match pair {
            case (lock, _)   => lock
        }

    pub def getCondition(pair: LockConditionPair): Condition = match pair {
            case (_, cond)   => cond
        }
        
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

    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Equivalent to the expression `c <- x`.
    ///
    pub def put(c: Channel[a], x: a): Channel[a] & Impure = c <- x

}
