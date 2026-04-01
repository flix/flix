package dev.flix.test;

/**
 * A generic interface with default methods.
 * Used to test NewObject with generic default method inheritance.
 */
public interface TestGenericDefaultMethods<T> {
    T transform(T input);

    default T transformTwice(T input) {
        return transform(transform(input));
    }

    default String asString(T value) {
        return value.toString();
    }
}
