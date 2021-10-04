namespace ChannelImpl {

    /**
    * The tuple corresponds to (id, channelLock, bufferSize, unbuffered, elementQueue, waitingGetters, waitingSetters)
    *   - id
    *   - channelLock is the channelLock of this channel.
    *   - bufferSize is the size of a channel. If you try to put an element in a channel that's full, you wait until there's space.
    *   - a flag for whether the channel is unbuffered or not
    *   - elementQueue is a circular buffer of elements in the list.
    *   - waitingGetters is a set of conditions that is waiting for get. This set is cleared after each new element.
    *   - waitingSetters is a condition that can notify threads of available space in the elementQueue.
    */
    pub enum ChannelImpl[a] {
        case Mpmc(
                Int64,
                ReentrantLock,
                Int,
                Bool,
                CircularBuffer[a],
                MutList[(ReentrantLock, Condition)],
                Condition
            )
    }

    pub def newWithId(bufferSize: Int, id: Int64): ChannelImpl[a] & Impure =
        let (bufSize, unbuffered) =
            if (bufferSize < 0) {
                bug!("Channel's buffer size must be non-negative.")
            } else if (bufferSize == 0)
                (1, true)
            else
                (bufferSize, false);

        let reentrantLock = ReentrantLock.new(false);

        Mpmc(
            id,
            reentrantLock,
            bufSize,
            unbuffered,
            CircularBuffer.new(bufSize),
            MutList.new(),
            ReentrantLock.newCondition(reentrantLock)
        )

    ///
    /// Returns a fresh buffered channel with internal capacity `c`.
    ///
    pub def new(bufferSize: Int): ChannelImpl[a] & Impure =
        newWithId(bufferSize, 0i64)

    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Equivalent to the expression `c <- x`.
    ///
    pub def put(c: ChannelImpl[a], x: a): ChannelImpl[a] & Impure =
        lock(c);

        // Check if the channel is full
        putHelperWaitOnFullChannel(c);

        // There was space to put another element in the channel
        let Mpmc(_, _, _, unbuffered, queue, waitingGetters, waitingSetters) = c;
        CircularBuffer.put(x, queue);

        // Signal waitingGetters that there is an element available
        MutList.foreach(
            (lockConditionPair) -> {
                let (conditionLock, condition) = lockConditionPair;
                ReentrantLock.lock(conditionLock);
                match Condition.signalAll(condition) {
                    case Ok(_) => ()
                    case Err(_) => bug!("Std lib error in ChannelImpl.flix")
                };
                match ReentrantLock.unlock(conditionLock) {
                    case Ok(_) => ()
                    case Err(_) => bug!("Std lib error in ChannelImpl.flix")
                }
            },
            waitingGetters);

        // Clear waitingGetters.
        // If a waitingGetter does not receive an element, it can add itself again
        MutList.clear!(waitingGetters);

        // If the channel is unbuffered, wait for the element to be handed off before continuing
        if (unbuffered)
            match Condition.awaitUninterruptibly(waitingSetters) {
                case Ok(_) => ()
                case Err(_) => bug!("Std lib error in ChannelImpl.flix")
            }
        else
            ();

        unlock(c);

        // Return the channel
        c

    def putHelperWaitOnFullChannel(c: ChannelImpl[a]): Unit & Impure =
        let Mpmc(_, _, bufferSize, _, queue, _, waitingSetters) = c;
        if (CircularBuffer.length(queue) >= bufferSize) {
            Condition.await(waitingSetters);
            putHelperWaitOnFullChannel(c)
        } else {
            ()
        }

    def lock(c: ChannelImpl[a]): Unit & Impure =
        let Mpmc(_, lock, _, _, _, _, _) = c;
        ReentrantLock.lock(lock)

    def unlock(chan: ChannelImpl[a]): Unit & Impure =
        let Mpmc(_, lock, _, _, _, _, _) = chan;
        match ReentrantLock.unlock(lock) {
            case Ok(_) => ()
            case Err(_illegalMonitorException) => bug!("Std lib error in ChannelImpl.flix")
        }

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Equivalent to the expression `<- c`.
    ///
    pub def get(chan: ChannelImpl[a]): a & Impure =
        lock(chan);

        let Mpmc(_, _, _, _, queue, _, waitingSetters) = chan;
        let optionalElement = CircularBuffer.tryGet(queue);
        let element = getHelperWaitOnEmptyChannel(chan, optionalElement);

        // Signal waiting setters that the channel has space
        Condition.signalAll(waitingSetters);

        unlock(chan);

        // Return the element from the channel
        element

    /*
    * Recursive helper function for get, loops and awaits on empty channel
    */
    def getHelperWaitOnEmptyChannel(chan: ChannelImpl[a], element: Option[a]): a & Impure =
        match element {
            case None => {
                let Mpmc(_, _, _, _, queue, waitingGetters, _) = chan;
                // No element was found

                // Create a new Lock and Condition
                let conditionLock = ReentrantLock.new(false);
                ReentrantLock.lock(conditionLock);

                let condition = ReentrantLock.newCondition(conditionLock);
                // Add LockConditionPair to the channel
                MutList.push!((conditionLock, condition), waitingGetters);

                // Temporarily unlock the channel while waiting. This is necessary as the Condition comes from a different Lock.
                unlock(chan);
                Condition.await(condition);
                lock(chan);

                // Someone signalled that an element was put in the channel.
                // Try to get the element (which could already be taken by someone else)
                let optionalElement = CircularBuffer.tryGet(queue);

                ReentrantLock.unlock(conditionLock);

                getHelperWaitOnEmptyChannel(chan, optionalElement)
            }
            case Some(e) => e
        }

    pub def tryGet(chan: ChannelImpl[a]): Option[a] & Impure =
        lock(chan);

        let Mpmc(_, _, _, _, queue, _, waitingSetters) = chan;
        // Try to get an element from the channel
        let element = CircularBuffer.tryGet(queue);

        // If there was an element, signal waiting setters
        match element {
            case None => ()
            case _ => {
                match Condition.signalAll(waitingSetters) {
                    case Ok(_) => ()
                    case Err(_) => bug!("Std lib error in ChannelImpl.flix")
                }
            }
        };

        unlock(chan);
        // Return the element from the channel, or None if channel was empty
        element

    def cmp(a: ChannelImpl[a], b: ChannelImpl[a]): Comparison =
        let Mpmc(a_id, _, _, _, _, _, _) = a;
        let Mpmc(b_id, _, _, _, _, _, _) = b;
        if (a_id == b_id)
            Comparison.fromInt(0)
        else if (a_id > b_id)
            Comparison.fromInt(1)
        else
            Comparison.fromInt(-1)

    def sortChannels(channels: Array[ChannelImpl[a]]): Array[ChannelImpl[a]] & Impure =
        Array.sortWith(cmp, channels)

    /**
    * Given a array of channels, returns the first channel that has an element
    * and return the index of that channel and the retrieved element in a
    * SelectChoice object.
    *
    * @param channels the channels to select on
    * @return the channel index of the channel with an element and the element
    */
    pub def selectImpl(channels: Array[ChannelImpl[a]], hasDefault: Bool): Option[(Int, a)] & Impure =
        // Create new Condition and channelLock the current thread
        let selectLock = ReentrantLock.new(false);
        let condition = ReentrantLock.newCondition(selectLock);

        // Sort channels to avoid deadlock when locking
        let sortedChannels = sortChannels(channels);
        selectHelper(channels, sortedChannels, selectLock, condition, hasDefault)

    def threadInterrupted(): Bool & Impure =
        import java.lang.Thread:interrupted();
        let isInterrupted = interrupted();
        if (isInterrupted)
            true
        else
            false

    def selectHelper(channels: Array[ChannelImpl[a]], sortedChannels: Array[ChannelImpl[a]], selectLock: ReentrantLock, condition: Condition, hasDefault: Bool): Option[(Int, a)] & Impure =
        if (not threadInterrupted()) {
            // Lock all channels in sorted order
            Array.foreach(lock, sortedChannels);

            // Lock the select lock after the channels
            ReentrantLock.lock(selectLock);

            // Find channels with waiting elements
            let selectChoice = selectChoiceWithWaitingElement(channels);

            match selectChoice {
                case Some((i, e)) => {
                    // Unlock all channels in sorted order, so other threads may input elements
                    Array.foreach(unlock, sortedChannels);

                    // Unlock the selectLock, which is relevant when a different thread wants to put
                    // an element into a channel that was not selected from the select.
                    // This other channel will then signal the condition from selectLock (in the put method),
                    // so it needs the lock.
                    ReentrantLock.unlock(selectLock);

                    Some((i, e))
                }
                case None => {
                    if (hasDefault) {
                        // Unlock all channels in sorted order, so other threads may input elements
                        Array.foreach(unlock, sortedChannels);

                        // Unlock the selectLock, which is relevant when a different thread wants to put
                        // an element into a channel that was not selected from the select.
                        // This other channel will then signal the condition from selectLock (in the put method),
                        // so it needs the lock.
                        ReentrantLock.unlock(selectLock);

                        // No element was found and default case exists
                        None
                    } else {
                        // No element was found.
                        // Add our condition to all channels to get notified when a new element is added
                        Array.foreach(
                            (c) -> {
                                addGetter(c, selectLock, condition)
                            },
                            channels);

                        // Unlock all channels in sorted order, so other threads may input elements
                        Array.foreach(unlock, sortedChannels);

                        // Wait for an element to be added to any of the channels
                        Condition.await(condition);

                        // Unlock the selectLock, which is relevant when a different thread wants to put
                        // an element into a channel that was not selected from the select.
                        // This other channel will then signal the condition from selectLock (in the put method),
                        // so it needs the lock.
                        ReentrantLock.unlock(selectLock);

                        selectHelper(channels, sortedChannels, selectLock, condition, hasDefault)
                    }
                }
            }
        } else {
            bug!("Thread is interrupted")
        }

    def selectChoiceWithWaitingElement(channels: Array[ChannelImpl[a]]): Option[(Int, a)] & Impure =
        selectChoiceWithWaitingElementHelper(0, channels)

    def selectChoiceWithWaitingElementHelper(index: Int, channels: Array[ChannelImpl[a]]): Option[(Int, a)] & Impure =
        if (index < 0) {
            bug!("Passed `index` less than 0")
        } else if (index >= Array.length(channels)) {
            None
        } else {
            match tryGet(channels[index]) {
                case None => selectChoiceWithWaitingElementHelper(index + 1, channels)
                case Some(element) => {
                    // There is a waiting element in this channel.
                    // Return the element and the branchNumber of this channel

                    // If an element found, add a new selectChoice to the MutList in order to later return
                    Some((index, element))
                }
            }
        }

    /**
    * Adds the given condition to the list of conditions waiting to
    * retrieve elements from the elementQueue.
    *
    * @param condition the condition to add
    */
    def addGetter(channel: ChannelImpl[a], conditionLock: ReentrantLock, condition: Condition): Unit & Impure =
        lock(channel);
        let pair = (conditionLock, condition);

        // Add LockConditionPair to the channel
        let Mpmc(_, _, _, _, _, waitingGetters, _) = channel;
        MutList.push!(pair, waitingGetters);
        unlock(channel)

}

pub enum CircularBuffer[a] {
    case CircularBuffer(
        Ref[Array[a]], //buf: array storing elements of the circular buffer
        Ref[Int], //startInd: ptr to the start of stored data
        Ref[Int], //endInd: ptr to the end of stored data
        Ref[Int] //count: current number of elements
    )
}

namespace CircularBuffer {
    ///
    /// Returns a new empty mutable list with a default capacity.
    ///
    pub def new(capacity: Int): CircularBuffer[a] & Impure =
        CircularBuffer(ref [default; capacity], ref 0, ref 0, ref 0)

    ///
    /// Returns the capacity of `cb`.
    ///
    pub def capacity(cb: CircularBuffer[a]): Int & Impure =
        let CircularBuffer(a, _, _, _) = cb;
        Array.length(deref a)

    ///
    /// Returns the first-in element in the circular buffer `cb`.
    ///
    pub def get(cb: CircularBuffer[a]): a & Impure =
        let CircularBuffer(a, si, _, l) = cb;
        let len = deref l;
        let startInd = deref si;
        if (isEmpty(cb)) {
            Console.printLine("Index out of bounds: the buffer is empty");
            (deref a)[-1] /// deliberately trigger index-out-of-bounds
        }
        else {
            l := len - 1;
            // Move the start index to start if at the end
            if (startInd < capacity(cb) - 1)
                si := startInd + 1
            else
                si := 0;
            Array.get(deref a, startInd)
        }

    ///
    /// Returns `true` if the given circular buffer `cb` is empty.
    ///
    pub def isEmpty(cb: CircularBuffer[a]): Bool & Impure =
        length(cb) == 0

    ///
    /// Returns `true` if the given circular buffer `cb` is full.
    ///
    pub def isFull(cb: CircularBuffer[a]): Bool & Impure =
        length(cb) == capacity(cb)

    ///
    /// Returns the number of elements in the given circular buffer `cb`.
    ///
    pub def length(cb: CircularBuffer[a]): Int & Impure =
        let CircularBuffer(_, _, _, l) = cb;
        deref l

    ///
    /// Inserts the given element `x` at the end of the given circular buffer `cb` and returns true if successful.
    ///
    pub def put(x: a, cb: mut CircularBuffer[a]): Bool & Impure =
        let CircularBuffer(a, _, ei, l) = cb;
        let len = deref l;
        let endInd = deref ei;

        if (isFull(cb)) false
        else {
            (deref a)[endInd] = x;
            l := len + 1;
            // Move the end index to start if at the end
            if (endInd < capacity(cb) - 1)
                ei := endInd + 1
            else
                ei := 0;
            true
        }

    ///
    /// Returns the first-in element in the circular buffer `cb` as an Optional type.
    ///
    pub def tryGet(cb: CircularBuffer[a]): Option[a] & Impure =
        let CircularBuffer(a, si, _, l) = cb;
        let len = deref l;
        let startInd = deref si;
        if (isEmpty(cb)) {
            None
        }
        else {
            l := len - 1;
            // Move the start index to start if at the end
            if (startInd < capacity(cb) - 1)
                si := startInd + 1
            else
                si := 0;
            Some(Array.get(deref a, startInd))
        }

}