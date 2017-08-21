/*
 * Copyright 2017 Ramin Zarifi
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

package ca.uwaterloo.flix.api.cell;

/**
 * Representation of a mutable Int64 cell.
 */
public final class Cell$Int64 {

    /**
     * The internal value.
     */
    private long value;

    /**
     * Constructs a new cell with the given value.
     */
    public Cell$Int64(long value) {
        this.value = value;
    }

    /**
     * Returns the value of the cell.
     */
    public long getValue() {
        return this.value;
    }

    /**
     * Sets the value of the cell.
     */
    public void setValue(long value) {
        this.value = value;
    }

    @Override
    public boolean equals(Object o) {
        throw new UnsupportedOperationException("Cell$Int64 does not support 'equals'.");
    }

    @Override
    public int hashCode() {
        throw new UnsupportedOperationException("Cell$Int64 does not support 'hashCode'.");
    }

    @Override
    public String toString() {
        throw new UnsupportedOperationException("Cell$Int64 does not support 'toString'.");
    }

}