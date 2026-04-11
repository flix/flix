package dev.flix.runtime;

/**
 * Internal wakeup used to abort a blocked JVM task when a region is being cancelled.
 *
 * This is not a user-visible exception. The uncaught-exception handler ignores it so that
 * a synthetic cancellation wakeup does not overwrite the real child/parent exception that
 * triggered region shutdown.
 */
public final class CancellationWakeup extends RuntimeException {

    public static final CancellationWakeup INSTANCE = new CancellationWakeup();

    private CancellationWakeup() {
        super(null, null, false, false);
    }
}
