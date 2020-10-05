/*
 * Copyright 2020 Andreas Salling Heglingegård
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.fixpoint.ram.interpreter;

import flix.runtime.ProxyObject;

import java.util.Arrays;

public final class Fact {
    private final ProxyObject[] elements;

    public Fact(ProxyObject[] elements) {
        if (elements == null)
            throw new IllegalArgumentException("'elements' must be non-null");
        this.elements = elements;
    }

    public ProxyObject getElement(int index) {
        if (index < 0 || index >= elements.length)
            throw new IndexOutOfBoundsException(index + " is not within the elements of the 'Fact'");
        return elements[index];
    }

    public int factSize(){
        return elements.length;
    }

    @Override
    public String toString() {
        return "Fact" + Arrays.toString(elements);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Fact fact = (Fact) o;
        return Arrays.equals(elements, fact.elements);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(elements);
    }
}
