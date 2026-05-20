package dev.flix.test;

/**
 * A generic interface with three type parameters.
 * Used to test NewObject with multi-parameter generics.
 */
public interface TestGenericInterface3<A, B, C> {
    C combine(A a, B b);
}
