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

    // todo: Make Channel a tuple instead of a record
    pub opaque type Channel[a] = {
        id: Int64,
        channelLock: ReentrantLock,
        bufferSize: Int,
        unbuffered: Bool,
        elementQueue: MutList[a],
        waitingGetters: MutSet[LockConditionPair],
        waitingSetters: Condition
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

        Channel({
            id = newId(),
            channelLock = reentrantLock,
            bufferSize = bufferSize,
            unbuffered = unbuffered,
            elementQueue = mutList,
            waitingGetters = MutSet.empty(),
            waitingSetters = newCondition(reentrantLock)
        })

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
