/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.runtime.value;

/**
 * A Java implementation of `FList`.
 * <p>
 * We use Java to ensure that we have absolute control over the structure of the object.
 */
public class FList {

    /**
     * A field for the head of the list.
     */
    private Object hd = null;

    /**
     * A field for the tail of the list.
     */
    private Object tl = null;

    /**
     * Constructs the list object.
     */
    public FList(Object hd, Object tl) {
        this.hd = hd;
        this.tl = tl;
    }

    /**
     * Returns the head of the list.
     */
    public Object getHd() {
        return hd;
    }

    /**
     * Returns the tail of the list.
     */
    public Object getTl() {
        return tl;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FList fList = (FList) o;

        return hd.equals(fList.hd) && tl.equals(fList.tl);

    }

    @Override
    public int hashCode() {
        int result = hd.hashCode();
        result = 31 * result + tl.hashCode();
        return result;
    }
}
