package dev.flix.test;

/**
 * A generic interface that extends TestGenericInterface, inheriting testMethod.
 * Also adds its own method.
 */
public interface TestGenericChildInterface<T> extends TestGenericInterface<T> {
    String describe();
}
