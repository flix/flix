package ca.uwaterloo.flix.runtime.interpreter;


import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;

public final class Channel {
  //TODO SJ: make docs
  private static AtomicInteger GLOBALCOUNTER = new AtomicInteger();

  private boolean isOpen = true;
  private LinkedList<Object> queue = new LinkedList<>();
  private Set<Condition> waitingGetters = new HashSet<>();
  private Lock lock = new ReentrantLock();
  private Condition waitingSetters = lock.newCondition();
  private int id = GLOBALCOUNTER.getAndIncrement();

  /**
   * NOT IMPLEMENTED
   */
  private int bufferSize;

  public void put(Object e) {
    lock.lock();

    try {
      checkIfClosed();
      queue.add(e);
      for (Condition c : waitingGetters) {
        c.signalAll();
      }
      waitingGetters.clear();
    } finally {
      lock.unlock();
    }
  }

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

  public Object tryGet() {
    lock.lock();

    try {
      //checkIfClosed(); you can read from a closed channel
      return queue.poll();
    } finally {
      lock.unlock();
    }
  }

  private void checkIfClosed() {
    if (!isOpen) throw new RuntimeException();
  }

  public void addGetter(Condition c) {
    lock.lock();
    try {
      waitingGetters.add(c);
    } finally {
      lock.unlock();
    }
  }

  public void close() {
    lock.lock();
    try {
      checkIfClosed();
      isOpen = false;
    } finally {
      lock.unlock();
    }
  }

  public int getId() {
    return id;
  }

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

  private static Channel[] sortChannels(Channel[] channels) {
    Channel[] channelsCopy = Arrays.copyOf(channels, channels.length);
    Arrays.sort(channelsCopy, Comparator.comparing((Channel c) -> c.id));
    return channelsCopy;
  }

  /**
   * TODO SJ rewrite this
   * PRECONDITION: Should always sort first to avoid deadlock.
   * @param channels
   */
  private static void lockAllChannels(Channel[] channels) {
    for (Channel c : channels) c.lock.lock();
  }

  private static void unlockAllChannels(Channel[] channels) {
    for (Channel c : channels) {
      c.lock.unlock();
    }
  }
}