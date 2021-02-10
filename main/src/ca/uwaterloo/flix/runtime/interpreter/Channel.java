package ca.uwaterloo.flix.runtime.interpreter;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;

// TODO: Move this to the Flix runtime.

/**
 * Precondition: Null is not a valid element to put in a channel.
 */
public final class Channel {
  /**
   * GLOBALCOUNTER gives a new id to every channel. Should not be
   * used directly, since every channel has an id field from it at creation.
   */
  private static AtomicInteger GLOBALCOUNTER = new AtomicInteger();

  /**
   * elementQueue is the elementQueue of elements in the list.
   */
  private LinkedList<Object> elementQueue = new LinkedList<>();

  /**
   * waitingGetters is a set of conditions that is waiting for get.
   * This set is cleared after each new element.
   */
  private Set<LockConditionPair> waitingGetters = new HashSet<>();

  /**
   * channelLock is the channelLock of this channel.
   */
  private Lock channelLock = new ReentrantLock();

  /**
   * waitingSetters is a condition that can notify threads of
   * available space in the elementQueue.
   */
  private Condition waitingSetters = channelLock.newCondition();

  /**
   * id is the unique identifier of a channel. This is used
   * to sort and channelLock a list of channels to avoid a deadlock.
   */
  private final int id = GLOBALCOUNTER.getAndIncrement();

  /**
   * bufferSize is the size of a channel. If you try to put an
   * element in a channel that's full, you wait until there's space.
   */
  private final int bufferSize;

  /**
   * a flag for whether the channel is unbuffered or not
   */
  private final boolean unbuffered;

  public Channel(int bufferSize) {
    if (bufferSize < 0) {
      throw new RuntimeException("Channel bufferSize must be positive");
    } else if (bufferSize == 0) {
      // Channel is unbuffered. Internally the size is 1
      this.bufferSize = 1;
      this.unbuffered = true;
    } else {
      // Channel is buffered with bufferSize.
      this.bufferSize = bufferSize;
      this.unbuffered = false;
    }
  }

  public static void spawn(Spawnable s) {
    // Create a new Thread and evaluate the spawned expression in the new Thread
    Thread thread = new Thread(s::spawn);
    thread.start();
  }

  /**
   * Given a array of channels, returns the first channel that has an element
   * and return the index of that channel and the retrieved element in a
   * SelectChoice object.
   *
   * @param channels the channels to select on
   * @return the channel index of the channel with an element and the element
   */
  public static SelectChoice select(Channel[] channels, boolean hasDefault) {
    // Create new Condition and channelLock the current thread
    Lock selectLock = new ReentrantLock();
    Condition condition = selectLock.newCondition();

    // Sort channels to avoid deadlock when locking
    Channel[] sortedChannels = sortChannels(channels);

    while (!Thread.interrupted()) {
      // Lock all channels in sorted order
      lockAllChannels(sortedChannels);
      try {
        // Lock the select lock after the channels
        selectLock.lock();

        try {
          // Check if any channel has an element
          for (int index = 0; index < channels.length; index++) {
            Channel channel = channels[index];
            Object element = channel.tryGet();
            if (element != null) {
              // Element found.
              // Return the element and the branchNumber (index of the array) of the containing channel
              return new SelectChoice(index, element);
            }
          }

          // No element was found.

          // If there is a default case, choose this
          if (hasDefault) {
            return SelectChoice.DEFAULT_CHOICE;
          }

          // Add our condition to all channels to get notified when a new element is added
          for (Channel channel : channels) {
            channel.addGetter(selectLock, condition);
          }
        } finally {
          // Unlock all channels in sorted order, so other threads may input elements
          unlockAllChannels(sortedChannels);
        }

        // Wait for an element to be added to any of the channels
        condition.await();
      } catch (InterruptedException e) {
        throw new RuntimeException("Thread interrupted");
      } finally {
        // Unlock the selectLock, which is relevant when a different thread wants to put
        // an element into a channel that was not selected from the select.
        // This other channel will then signal the condition from selectLock (in the put method),
        // so it needs the lock.
        selectLock.unlock();
      }
    }

    throw new RuntimeException("Thread interrupted");
  }

  /**
   * Returns a new array of the given channels sorted by their id.
   * The given array is not changed.
   *
   * @param channels the channels so sort
   * @return a sorted array of the given channels
   */
  private static Channel[] sortChannels(Channel[] channels) {
    // Create a new array to sort since the original is still used
    Channel[] channelsCopy = Arrays.copyOf(channels, channels.length);
    Arrays.sort(channelsCopy, Comparator.comparing((Channel c) -> c.id));
    return channelsCopy;
  }

  /**
   * Locks all of the given channels. The given list of channels
   * should always be sorted first to avoid deadlocks.
   * PRECONDITION: The channel array should be sorted by id.
   *
   * @param channels the array of channels to unlock
   */
  private static void lockAllChannels(Channel[] channels) {
    for (Channel c : channels) c.channelLock.lock();
  }

  /**
   * unlocks all the given channels.
   *
   * @param channels the channels to unlock
   */
  private static void unlockAllChannels(Channel[] channels) {
    // Unlock channels in reverse order like Go.
    for (int i = channels.length - 1; i >= 0; i--) {
      channels[i].channelLock.unlock();
    }
  }

  /**
   * Put an element into the elementQueue or wait until it is possible
   * to do so.
   *
   * @param e the element to add
   */
  public void put(Object e) {
    channelLock.lock();

    try {
      // Check if the channel is full
      while (elementQueue.size() >= bufferSize) {
        if (Thread.interrupted()) {
          throw new RuntimeException("Thread interrupted");
        }

        // Wait until signalled that the channel has space
        try {
          waitingSetters.await();
        } catch (InterruptedException e1) {
          throw new RuntimeException("Thread interrupted");
        }
      }

      // There was space to put another element in the channel
      elementQueue.add(e);

      // Signal waitingGetters that there is an element available
      for (LockConditionPair pair : waitingGetters) {
        Lock conditionLock = pair.getLock();
        Condition condition = pair.getCondition();
        try {
          conditionLock.lock();
          condition.signalAll();
        } finally {
          conditionLock.unlock();
        }
      }
      // Clear waitingGetters.
      // If a waitingGetter does not receive an element, it can add itself again
      waitingGetters.clear();

      // If the channel is unbuffered, wait for the element to be handed off before continuing
      if (unbuffered) {
        try {
          waitingSetters.await();
        } catch (InterruptedException e1) {
          throw new RuntimeException("Thread interrupted");
        }
      }
    } finally {
      channelLock.unlock();
    }
  }

  /**
   * Retrieves the head of the elementQueue or waits until it is possible to do so.
   *
   * @return the head of the elementQueue
   */
  public Object get() {
    channelLock.lock();
    try {
      // Try to get an element
      Object e = elementQueue.poll();
      while (e == null) {
        // No element was found

        // Create a new Lock and Condition
        Lock conditionLock = new ReentrantLock();
        conditionLock.lock();
        try {
          Condition condition = conditionLock.newCondition();
          // Add LockConditionPair to the channel
          waitingGetters.add(new LockConditionPair(conditionLock, condition));
          // Temporarily unlock the channel while waiting. This is necessary as the Condition comes from a different Lock.
          try {
            channelLock.unlock();
            condition.await();
          } finally {
            channelLock.lock();
          }

          // Someone signalled that an element was put in the channel.
          // Try to get the element (which could already be taken by someone else)
          e = elementQueue.poll();
        } finally {
          conditionLock.unlock();
        }
      }
      // Signal waiting setters that the channel has space
      waitingSetters.signalAll();

      // Return the element from the channel
      return e;
    } catch (InterruptedException e2) {
      throw new RuntimeException();
    } finally {
      channelLock.unlock();
    }
  }

  /**
   * Retrieves the head of the elementQueue or null if the elementQueue is empty.
   *
   * @return the head of the elementQueue or null if its empty
   */
  public Object tryGet() {
    channelLock.lock();

    try {
      // Try to get an element from the channel
      Object element = elementQueue.poll();
      // If there was an element, signal waiting setters
      if (element != null) {
        waitingSetters.signalAll();
      }
      // Return the element from the channel, or null if channel was empty
      return element;
    } finally {
      channelLock.unlock();
    }
  }

  /**
   * Adds the given condition to the list of conditions waiting to
   * retrieve elements from the elementQueue.
   *
   * @param condition the condition to add
   */
  public void addGetter(Lock conditionLock, Condition condition) {
    channelLock.lock();
    try {
      waitingGetters.add(new LockConditionPair(conditionLock, condition));
    } finally {
      channelLock.unlock();
    }
  }

  /**
   * Returns the unique id of the channel.
   *
   * @return the unique id of the channel
   */
  public int getId() {
    return id;
  }
}
