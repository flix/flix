package ca.uwaterloo.flix.runtime.interpreter;


import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;
import java.util.function.Function;

//TODO SJ: make docs
public final class Channel {

  /**
   * GLOBALCOUNTER gives a new id to every channel. Should not be
   * used directly, since every channel has an id field from it at creation.
   */
  private static AtomicInteger GLOBALCOUNTER = new AtomicInteger();
  /**
   * isOpen indicates whether or not the channel can recieve any more element.
   * A runtime exception is thrown if you put on a closed channel.
   * Get works normally.
   */
  private boolean isOpen = true;
  /**
   *  queue is the queue of elements in the list.
   */
  private LinkedList<Object> queue = new LinkedList<>();
  /**
   * waitingGetters is a set of conditions that is waiting for get.
   * This set is cleared after each new element.
   */
  private Set<Condition> waitingGetters = new HashSet<>();
  /**
   * lock is the lock of this channel.
   */
  private Lock lock = new ReentrantLock();
  /**
   * NOT IMPLEMENTED.
   * waitingSetters is a condition that can notify threads of
   * available space in the queue. This is useless without bufferSize.
   */
  private Condition waitingSetters = lock.newCondition();
  /**
   * id is the unique identifier of a channel. This is used
   * to sort and lock a list of channels to avoid a deadlock.
   */
  private final int id = GLOBALCOUNTER.getAndIncrement();

  /**
   * NOT IMPLEMENTED.
   * bufferSize is the size of a channel. If you try to put an
   * element in a channel that's full, you wait until there's space.
   */
  private int bufferSize;

  public static void spawn(Spawnable s) {
    new Thread(){
      @Override
      public void run() {
        s.spawn();
      }
    }.start();
  }

  /**
   * Put an element into the queue or wait until it is possible
   * to do so.
   * @param e the element to add
   */
  public void put(Object e) {
    lock.lock();

    try {
      checkIfClosed();
      // TODO SJ: implement bufferSize here
      queue.add(e);
      for (Condition c : waitingGetters) {
        c.signalAll();
      }
      waitingGetters.clear();
    } finally {
      lock.unlock();
    }
  }

  /**
   * Retrieves the head of the queue or waits until it is possible to do so.
   * @return the head of the queue
   */
  public Object get() {
    lock.lock();
    try {
      //checkIfClosed(); you can read from a closed channel
      Object e = queue.poll();
      while (e == null) {
        Condition c = lock.newCondition();
        waitingGetters.add(c);
        c.await();
        e = queue.poll();
      }
      return e;
    } catch (InterruptedException e2) {
      throw new RuntimeException();
    } finally {
      lock.unlock();
    }
  }

  /**
   * Retrieves the head of the queue or null if the queue is empty.
   * @return the head of the queue or null if its empty
   */
  public Object tryGet() {
    lock.lock();

    try {
      //checkIfClosed(); you can read from a closed channel
      return queue.poll();
    } finally {
      lock.unlock();
    }
  }

  /**
   * If isOpen is false throw a runtime exception.
   */
  private void checkIfClosed() {
    if (!isOpen) throw new RuntimeException();
  }

  /**
   * Adds the given condition to the list of conditions waiting to
   * retrieve elements from the queue.
   * @param c the condition to add
   */
  public void addGetter(Condition c) {
    lock.lock();
    try {
      waitingGetters.add(c);
    } finally {
      lock.unlock();
    }
  }

  /**
   * Closes the channel. All subsequent put calls will throw a runtime exception.
   * If the channel is already closed a runtime exception will also be thrown.
   */
  public void close() {
    lock.lock();
    try {
      checkIfClosed();
      isOpen = false;
    } finally {
      lock.unlock();
    }
  }

  /**
   * Returns the unique id of the channel.
   * @return the unique id of the channel
   */
  public int getId() {
    return id;
  }

  /**
   * Given a array of channels, returns the first channel that has an element
   * and return the index of that channel and the retrieved element in a
   * SelectChoice object.
   * @param channels the channels to select on
   * @return the channel index of the channel with an element and the element
   */
  public static SelectChoice select(Channel[] channels) {
    // Create new Condition and lock the current thread
    Lock selectLock = new ReentrantLock();
    Condition condition = selectLock.newCondition();
    selectLock.lock();

    // Sort Channels to avoid deadlock when locking
    Channel[] sortedChannels = sortChannels(channels);

    while (!Thread.interrupted()) {
      // Lock all Channels in sorted order
      lockAllChannels(sortedChannels);

      try {
        // Check if any Channel has an element
        for (int index = 0; index < channels.length; index++) {
          Channel channel = channels[index];
          Object element = channel.tryGet();
          if (element != null) {
            // Element found.
            // Return the element and the branchNumber (index) of the containing Channel
            SelectChoice choice = new SelectChoice();
            choice.branchNumber = index;
            choice.element = element;
            return choice;
          }
        }

        // No element was found.
        // Add our condition to all Channels to get notified when a new element is added
        for (Channel channel : channels) {
          channel.addGetter(condition);
        }
      } finally {
        // Unlock all Channels in sorted order, so other threads may input elements
        unlockAllChannels(sortedChannels);
      }

      // Wait for an element to be added to any of the Channels
      try {
        condition.await();
      } catch (InterruptedException e) {
        throw new RuntimeException("Thread interrupted");
      }
    }

    throw new RuntimeException("Thread interrupted");
  }

  /**
   * Returns a new array of the given channels sorted by their id.
   * The given array is not changed.
   * @param channels the channels so sort
   * @return a sorted array of the given channels
   */
  private static Channel[] sortChannels(Channel[] channels) {
    Channel[] channelsCopy = Arrays.copyOf(channels, channels.length);
    Arrays.sort(channelsCopy, Comparator.comparing((Channel c) -> c.id));
    return channelsCopy;
  }

  /**
   * Locks all of the given channels. The given list of channels
   * should always be sorted first to avoid deadlocks.
   * PRECONDITION: The channel array should be sorted by id.
   * @param channels the array of channels to unlock
   */
  private static void lockAllChannels(Channel[] channels) {
    for (Channel c : channels) c.lock.lock();
  }

  /**
   * unlocks all the given channels.
   * @param channels the channels to unlock
   */
  private static void unlockAllChannels(Channel[] channels) {
    for (Channel c : channels) {
      c.lock.unlock();
    }
  }
}