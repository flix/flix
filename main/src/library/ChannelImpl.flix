namespace ChannelImpl {

    /**
    * The tuple corresponds to (id, channelLock, bufferType, elementQueue, waitingGetters, waitingSetters)
    *   - id
    *   - channelLock is the channelLock of this channel.
    *   - buffered indicates whether a handoff is needed or not. If you try to put an element in a channel that's full, you wait until there's space.
    *   - maxQueueSize is the max elements allowed in the queue. This is max(1, channel size).
    *   - elementQueue is a circular buffer of elements in the list, if bufferSize is 0 the the queue is size 1.
    *   - waitingGetters is a set of conditions that is waiting for get. This set is cleared after each new element.
    *   - waitingSetters is a condition that can notify threads of available space in the elementQueue.
    */
    pub enum ChannelImpl[a] {
        case Mpmc(
                Int64,
                ReentrantLock,
                Bool,
                Int,
                MutDeque[a],
                MutList[(ReentrantLock, Condition)],
                Condition
            )
    }

    pub def new(bufferSize: Int): Result[ChannelImpl[a], String] & Impure =
        if (bufferSize < 0) Err("Channel's buffer size must be non-negative.")
        else Ok(uncheckedNew(bufferSize))

    pub def uncheckedNew(bufferSize: Int): ChannelImpl[a] & Impure =
        import dev.flix.runtime.GlobalCounter:newId();

        let buffered = bufferSize == 0;

        let maxQueueSize = if (buffered) 1 else bufferSize;

        let reentrantLock = ReentrantLock.new(false);

        Mpmc(
            newId(),
            reentrantLock,
            buffered,
            maxQueueSize,
            MutDeque.new(maxQueueSize),
            MutList.new(),
            ReentrantLock.newCondition(reentrantLock)
        )

    ///
    /// Sends the element `x` on the channel `c`.
    ///
    /// Implements the expression `c <- x`.
    ///
    pub def put(c: ChannelImpl[a], x: a): ChannelImpl[a] & Impure =
        let Mpmc(_, channelLock, buffered, _, queue, waitingGetters, waitingSetters) = c;
        ReentrantLock.lock(channelLock);

        // Block until the channel is not full
        awaitAvailableSpace(c);

        // Insert the new element
        MutDeque.pushBack!(x, queue);

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
        c

    ///
    /// The channel lock is expected to be held.
    ///
    def awaitAvailableSpace(c: ChannelImpl[a]): Unit & Impure =
        let Mpmc(_, _, _, maxQueueSize, queue, _, waitingSetters) = c;
        
        if (MutDeque.size(queue) == maxQueueSize) {
            awaitCondition(waitingSetters);
            awaitAvailableSpace(c)
        } else {
            ()
        }

    ///
    /// Receives an element from the channel `c`.
    ///
    /// Implements to the expression `<- c`.
    ///
    pub def get(chan: ChannelImpl[a]): a & Impure =
        let Mpmc(_, channelLock, _, _, queue, _, waitingSetters) = chan;
        ReentrantLock.lock(channelLock);
        // Pop the next element
        let optionalElement = MutDeque.popFront!(queue);
        let element = getHelper(chan, optionalElement);

        // Signal waiting setters that the channel has space
        signalCondition(waitingSetters);

        unlockLock(channelLock);
        element

    ///
    /// Recursive helper function for get, loops and awaits on empty channel.
    /// The channel lock is expected to be held.
    ///
    def getHelper(chan: ChannelImpl[a], element: Option[a]): a & Impure =
        match element {
            case None => {
                let Mpmc(_, channelLock, _, _, queue, waitingGetters, _) = chan;
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
                // We still hold the condtion lock so there is no race here
                awaitCondition(condition);
                ReentrantLock.lock(channelLock);

                // Someone signalled that an element was put in the channel.
                // Try to get the element (which could already be taken by someone else)
                let optionalElement = MutDeque.popFront!(queue);

                unlockLock(conditionLock);

                getHelper(chan, optionalElement)
            }
            case Some(e) => e
        }

    pub def tryGet(chan: ChannelImpl[a]): Option[a] & Impure =
        let Mpmc(_, channelLock, _, _, queue, _, waitingSetters) = chan;
        ReentrantLock.lock(channelLock);

        // Try to get an element from the channel
        let element = MutDeque.popFront!(queue);

        // If there was an element, signal waiting setters
        match element {
            case None => ()
            case _ => signalCondition(waitingSetters)
        };

        unlockLock(channelLock);
        // Return the element from the channel, or None if channel was empty
        element

    def cmp(a: ChannelImpl[a], b: ChannelImpl[a]): Comparison =
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
    pub def selectImpl(channels: Array[ChannelImpl[a]], hasDefault: Bool): Option[(Int, a)] & Impure =
        // Create new Condition and channelLock the current thread
        let selectLock = ReentrantLock.new(false);
        let selectCondition = ReentrantLock.newCondition(selectLock);

        // Sort channels to avoid deadlocks
        let sortedChannels = Array.sortWith(cmp, channels);
        selectHelper(channels, sortedChannels, selectLock, selectCondition, hasDefault)

    def threadInterrupted(): Bool & Impure =
        import java.lang.Thread:interrupted();
        interrupted()

    def selectHelper(channels: Array[ChannelImpl[a]], sortedChannels: Array[ChannelImpl[a]], selectLock: ReentrantLock, selectCondition: Condition, hasDefault: Bool): Option[(Int, a)] & Impure =
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
            bug!("Thread is interrupted")
        }

    def firstAvailableElement(index: Int, channels: Array[ChannelImpl[a]]): Option[(Int, a)] & Impure =
        if (index < 0) {
            // Passed `index` less than 0
            bug!("Std lib error in ChannelImpl.flix")
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

    ///
    /// The channel lock is expected to be held.
    ///
    def awaitCondition(c: Condition): Unit & Impure =
        match Condition.awaitUninterruptibly(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await wihout holding the corresponding lock
                bug!("Std lib error in ChannelImpl.flix")
        }

    ///
    /// The condition lock is expected to be held.
    ///
    def signalCondition(c: Condition): Unit & Impure =
        match Condition.signalAll(c) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: await wihout holding the corresponding lock
                bug!("Std lib error in ChannelImpl.flix")
        }

    ///
    /// The lock is expected to be held.
    ///
    def unlockLock(l: ReentrantLock): Unit & Impure =
        match ReentrantLock.unlock(l) {
            case Ok(_) => ()
            case Err(_) =>
                // Error: unluck without holding the lock
                bug!("Std lib error in ChannelImpl.flix")
        }

}

pub opaque type MutDeque[_a] = Unit

namespace MutDeque {

    pub def new(capacity: Int): MutDeque[a] & Impure = ???

    pub def size(md: MutDeque[a]): Int = ???

    pub def pushBack!(x: a, md: mut MutDeque[a]): Unit & Impure = ???

    pub def popFront!(md: mut MutDeque[a]): Option[a] & Impure = ???

}
