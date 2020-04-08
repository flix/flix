package flix.runtime.fixpoint.ram.interpreter;

import flix.runtime.ProxyObject;

import java.util.Arrays;

public final class Fact {
    private final ProxyObject[] elements;

    public Fact(ProxyObject[] elements) {
        if (elements == null || elements.length < 1)
            throw new IllegalArgumentException("'elements' must be non-null and non-empty");
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
