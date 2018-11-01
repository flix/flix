package ca.uwaterloo.flix.runtime.interpreter;


import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;

public class Channel {
  private boolean isOpen;
  private int bufferSize;
  private LinkedList<Object> queue;
  private Set<Condition> waitingGetters;
  private Condition waitingSetters;
  private Lock lock;
  private static AtomicInteger GLOBALCOUNTER = new AtomicInteger();
  private int id;

  public Channel() {
    this.isOpen = true;
    this.queue = new LinkedList<>();
    this.lock = new ReentrantLock();
    this.waitingGetters = new HashSet<>();
    this.waitingSetters = lock.newCondition();
    this.id = GLOBALCOUNTER.getAndIncrement();
  }

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

    // Sort Channels to avoid deadlock
    sortChannels(channels);

    while (!Thread.interrupted()) {
      // Lock (and sort) all Channels
      lockAllChannels(channels);

      try {
        // Check if any Channel has an element
        for (Channel channel : channels) {
          Object element = channel.tryGet();
          if (element != null) {
            // Element found.
            // Return the element and the index of the containing Channel
            SelectChoice choice = new SelectChoice();
            choice.channelId = channel.id;
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
        // Unlock all Channels so other threads may input elements
        unlockAllChannels(channels);
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

  private static void sortChannels(Channel[] channels) {
    Arrays.sort(channels, Comparator.comparing((Channel c) -> c.id));
  }

  private static void lockAllChannels(Channel[] channels) {
    // Arrays.sort(channels, Comparator.comparing((Channel c) -> c.id));
    for (Channel c : channels) c.lock.lock();
  }

  private static void unlockAllChannels(Channel[] channels) {
    for (Channel c : channels) {
      c.lock.unlock();
    }
  }
}