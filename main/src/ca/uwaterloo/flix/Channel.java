package ca.uwaterloo.flix;

import java.util.*;
import java.util.concurrent.locks.*;

public class Channel {
    private Queue<Object> queue;
    private Integer capacity;
    private Lock lock;
    private Condition channelNotFull;
    private Condition channelNotEmpty;
    private List<SelectChannel> selects;

    // New Channel
    public Channel(Integer capacity) {
        this.capacity = capacity;
        this.queue = new LinkedList<>();
        this.lock = new ReentrantLock();
        this.channelNotFull = lock.newCondition();
        this.channelNotEmpty = lock.newCondition();
        this.selects = new ArrayList<>();
    }

    // Get Channel
    public Object get() {
        Object value = null;

        lock();
        try {
            while (isEmpty()) {
                awaitNotFull();
            }

            value = poll();

            if (value != null) {
                signalNotEmpty();
            }
        }
        catch (InterruptedException ex) {
        }
        finally {
            unlock();
        }

        return value;
    }

    // Put Channel
    public Channel put(Object value) throws InterruptedException {
        lock();
        try {
            while (isFull()) {
                awaitNotEmpty();
            }

            offer(value);
            signalNotFull();

            signalAllSelects();
            clearSelects();
        }
        finally {
            unlock();
        }

        return this;
    }

    public Object poll() {
        return queue.poll();
    }

    public Object offer(Object value) {
        return queue.offer(value);
    }

    public Boolean isEmpty() {
        return queue.isEmpty();
    }

    public Boolean nonEmpty() {
        return !isEmpty();
    }

    public Boolean isFull() {
        return size().equals(capacity);
    }

    public Integer size() {
        return queue.size();
    }

    public void lock() {
        lock.lock();
    }

    public void unlock() {
        lock.unlock();
    }

    public void awaitNotFull() throws InterruptedException {
        channelNotFull.await();
    }

    public void awaitNotEmpty() throws InterruptedException {
        channelNotEmpty.await();
    }

    public void signalNotFull() {
        channelNotFull.signalAll();
    }

    public void signalNotEmpty() {
        channelNotEmpty.signalAll();
    }

    public void addSelect(SelectChannel select) {
        selects.add(select);
    }

    private void signalAllSelects() {
        for (SelectChannel select : selects) {
            select.lock();
            try {
                select.signal();
            }
            finally {
                select.unlock();
            }
        }
    }

    private void clearSelects() {
        selects.clear();
    }
}
