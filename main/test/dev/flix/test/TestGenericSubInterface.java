package dev.flix.test;

/**
 * A generic sub-interface that extends Comparable, inheriting the compareTo method.
 * Used to test NewObject with inherited generic methods.
 */
public interface TestGenericSubInterface<T extends Comparable<T>> extends Comparable<T> {
    T getValue();
}
