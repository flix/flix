/*
 * Copyright 2018 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
