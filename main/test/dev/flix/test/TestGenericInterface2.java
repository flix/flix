package dev.flix.test;

/**
 * A generic interface with two type parameters.
 * Used to test NewObject with multi-parameter generics.
 */
public interface TestGenericInterface2<A, B> {
    B apply(A a);
}
