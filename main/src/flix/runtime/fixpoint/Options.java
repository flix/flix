package flix.runtime.fixpoint;

import java.time.Duration;

public final class Options {

    /**
     * Whether to enable to fixpoint monitor.
     */
    private boolean monitored = false;

    /**
     * The number of threads to use during fixpoint computation.
     */
    private int threads = 1;

    /**
     * The timeout to use during fixpoint computation.
     */
    private Duration timeout = null;

    /**
     * Whether to print verbose information during fixpoint computation.
     */
    private boolean verbose = false;

    /**
     * Returns `true` if the fixpoint computation should be monitored.
     */
    public boolean isMonitored() {
        return monitored;
    }

    /**
     * Sets whether the fixpoint computation should be monitored.
     */
    public void setMonitored(boolean monitored) {
        this.monitored = monitored;
    }

    /**
     * Returns the number of threads to use for the fixpoint computation.
     */
    public int getThreads() {
        return threads;
    }

    /**
     * Sets the number of threads to use for the fixpoint computation.
     */
    public void setThreads(int threads) {
        this.threads = threads;
    }

    /**
     * Returns the timeout to use for the fixpoint computation (if any).
     */
    public Duration getTimeout() {
        return timeout;
    }

    /**
     * Sets the timeout to use for the fixpoint computation (if any).
     */
    public void setTimeout(Duration timeout) {
        this.timeout = timeout;
    }

    /**
     * Returns whether the fixpoint computation should print verbose information.
     */
    public boolean isVerbose() {
        return verbose;
    }

    /**
     * Sets whether the fixpoint computation should print verbose information.
     */
    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

}
