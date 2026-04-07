package dev.flix.test;

/**
 * A generic abstract class with one type parameter.
 * Used to test NewObject extending generic abstract classes.
 */
public abstract class TestGenericAbstractClass<T> {
    public T value;

    public TestGenericAbstractClass(T value) {
        this.value = value;
    }

    public T getValue() {
        return value;
    }

    public abstract T transform(T input);

    public String describe() {
        return "value=" + value.toString();
    }
}
