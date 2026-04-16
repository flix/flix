package dev.flix.test;

/**
 * A generic interface with a bounded type parameter.
 * Used to test NewObject with bounded generics.
 */
public interface TestBoundedGenericInterface<T extends Comparable<T>> {
    T getMin(T a, T b);
    T getMax(T a, T b);
}
