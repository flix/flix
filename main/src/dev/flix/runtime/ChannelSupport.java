package dev.flix.runtime;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Portable channel runtime support for the JVM backend.
 *
 * This is intentionally separate from the legacy JVM stdlib Concurrent/Channel implementation.
 * The portable stdlib profile should share the same channel/select contract as LLVM backends.
 */
public final class ChannelSupport {

    private static final AtomicLong NEXT_CHANNEL_ID = new AtomicLong(1L);
    private static final AtomicLong NEXT_SELECT_TOKEN = new AtomicLong(1L);
    private static final ConcurrentHashMap<Long, SelectToken> SELECT_TOKENS = new ConcurrentHashMap<>();
    private static final Object NO_VALUE = new Object();

    private ChannelSupport() {
    }

    public static Object newChannel(int capacity) {
        return new Channel(capacity);
    }

    public static void put(Object channel, Object value) {
        ((Channel) channel).put(value);
    }

    public static Object take(Object channel) {
        return ((Channel) channel).take();
    }

    public static long select(Object[] channelArray, boolean blocking) {
        Objects.requireNonNull(channelArray, "channelArray");
        final Channel[] channels = new Channel[channelArray.length];
        for (int i = 0; i < channelArray.length; i++) {
            channels[i] = (Channel) channelArray[i];
        }

        final Channel[] uniqueChannels = uniqueSorted(channels);
        final SelectWaiter waiter = new SelectWaiter();

        while (true) {
            lockAll(uniqueChannels);
            try {
                final SelectResult result = trySelectLocked(channels);
                if (result != null) {
                    return result.token;
                }
                if (!blocking) {
                    return 0L;
                }
                for (Channel channel : uniqueChannels) {
                    channel.selectWaiters.add(waiter);
                }
            } finally {
                unlockAll(uniqueChannels);
            }

            boolean cancelled = false;
            try {
                waiter.awaitSignal();
            } catch (CancellationWakeup e) {
                cancelled = true;
            }
            lockAll(uniqueChannels);
            try {
                for (Channel channel : uniqueChannels) {
                    channel.selectWaiters.removeIf(w -> w == waiter);
                }
                waiter.reset();
            } finally {
                unlockAll(uniqueChannels);
            }

            if (cancelled) {
                throw CancellationWakeup.INSTANCE;
            }
        }
    }

    public static int selectIndex(long token) {
        if (token == 0L) {
            return -1;
        }

        final SelectToken selectToken = SELECT_TOKENS.get(token);
        if (selectToken == null) {
            throw new RuntimeException("Invalid channel select token: " + token);
        }
        return selectToken.index;
    }

    public static Object getSelected(long token) {
        final SelectToken payload = SELECT_TOKENS.remove(token);
        if (payload == null) {
            throw new RuntimeException("Invalid channel select token: " + token);
        }
        return payload.payload;
    }

    private static SelectResult trySelectLocked(Channel[] channels) {
        for (int i = 0; i < channels.length; i++) {
                final Object payload = channels[i].tryTakeLocked();
                if (payload != NO_VALUE) {
                    final long token = NEXT_SELECT_TOKEN.getAndIncrement();
                    SELECT_TOKENS.put(token, new SelectToken(i, payload));
                    return new SelectResult(i, token);
                }
            }
        return null;
    }

    private static Channel[] uniqueSorted(Channel[] channels) {
        final List<Channel> unique = new ArrayList<>(channels.length);
        final IdentityHashMap<Channel, Boolean> seen = new IdentityHashMap<>();
        for (Channel channel : channels) {
            if (!seen.containsKey(channel)) {
                seen.put(channel, Boolean.TRUE);
                unique.add(channel);
            }
        }
        unique.sort(Comparator.comparingLong(c -> c.id));
        return unique.toArray(new Channel[0]);
    }

    private static void lockAll(Channel[] channels) {
        for (Channel channel : channels) {
            channel.lock.lock();
        }
    }

    private static void unlockAll(Channel[] channels) {
        for (int i = channels.length - 1; i >= 0; i--) {
            channels[i].lock.unlock();
        }
    }

    private static void awaitOrCancel(Condition condition) {
        try {
            condition.await();
        } catch (InterruptedException e) {
            throw CancellationWakeup.INSTANCE;
        }
    }

    private static final class SelectResult {
        private final int index;
        private final long token;

        private SelectResult(int index, long token) {
            this.index = index;
            this.token = token;
        }
    }

    private static final class SelectToken {
        private final int index;
        private final Object payload;

        private SelectToken(int index, Object payload) {
            this.index = index;
            this.payload = payload;
        }
    }

    private static final class SelectWaiter {
        private final ReentrantLock lock = new ReentrantLock();
        private final Condition condition = lock.newCondition();
        private boolean signaled = false;

        private void signal() {
            lock.lock();
            try {
                signaled = true;
                condition.signalAll();
            } finally {
                lock.unlock();
            }
        }

        private void awaitSignal() {
            lock.lock();
            try {
                while (!signaled) {
                    awaitOrCancel(condition);
                }
            } finally {
                lock.unlock();
            }
        }

        private void reset() {
            signaled = false;
        }
    }

    private static final class Channel {
        private final long id = NEXT_CHANNEL_ID.getAndIncrement();
        private final int capacity;
        private final ReentrantLock lock = new ReentrantLock();
        private final Condition notEmpty = lock.newCondition();
        private final Condition notFull = lock.newCondition();
        private final ArrayDeque<Object> queue = new ArrayDeque<>();
        private final List<SelectWaiter> selectWaiters = new ArrayList<>();

        private boolean rendezvousHasValue = false;
        private Object rendezvousValue = null;

        private Channel(int capacity) {
            if (capacity < 0) {
                throw new IllegalArgumentException("capacity < 0");
            }
            this.capacity = capacity;
        }

        private void put(Object value) {
            lock.lock();
            try {
                if (capacity == 0) {
                    while (rendezvousHasValue) {
                        awaitOrCancel(notFull);
                    }

                    rendezvousValue = value;
                    rendezvousHasValue = true;
                    notEmpty.signal();
                    signalSelectWaitersLocked();

                    while (rendezvousHasValue) {
                        try {
                            notFull.await();
                        } catch (InterruptedException e) {
                            if (rendezvousHasValue) {
                                rendezvousValue = null;
                                rendezvousHasValue = false;
                                notFull.signal();
                                throw CancellationWakeup.INSTANCE;
                            }
                            return;
                        }
                    }
                    return;
                }

                while (queue.size() == capacity) {
                    awaitOrCancel(notFull);
                }

                queue.addLast(value);
                notEmpty.signal();
                signalSelectWaitersLocked();
            } finally {
                lock.unlock();
            }
        }

        private Object take() {
            lock.lock();
            try {
                if (capacity == 0) {
                    while (!rendezvousHasValue) {
                        awaitOrCancel(notEmpty);
                    }

                    final Object value = rendezvousValue;
                    rendezvousValue = null;
                    rendezvousHasValue = false;
                    notFull.signal();
                    return value;
                }

                while (queue.isEmpty()) {
                    awaitOrCancel(notEmpty);
                }

                final Object value = queue.removeFirst();
                notFull.signal();
                return value;
            } finally {
                lock.unlock();
            }
        }

        private Object tryTakeLocked() {
            if (capacity == 0) {
                if (!rendezvousHasValue) {
                    return NO_VALUE;
                }

                final Object value = rendezvousValue;
                rendezvousValue = null;
                rendezvousHasValue = false;
                notFull.signal();
                return value;
            }

            if (queue.isEmpty()) {
                return NO_VALUE;
            }

            final Object value = queue.removeFirst();
            notFull.signal();
            return value;
        }

        private void signalSelectWaitersLocked() {
            if (selectWaiters.isEmpty()) {
                return;
            }

            final SelectWaiter[] snapshot = selectWaiters.toArray(new SelectWaiter[0]);
            selectWaiters.clear();
            for (SelectWaiter waiter : snapshot) {
                waiter.signal();
            }
        }
    }
}
