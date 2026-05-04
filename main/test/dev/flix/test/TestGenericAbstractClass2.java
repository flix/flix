package dev.flix.test;

/**
 * A generic abstract class with two type parameters.
 * Used to test NewObject extending multi-param generic abstract classes.
 */
public abstract class TestGenericAbstractClass2<K, V> {
    public K key;
    public V val;

    public TestGenericAbstractClass2(K key, V val) {
        this.key = key;
        this.val = val;
    }

    public K getKey() {
        return key;
    }

    public V getVal() {
        return val;
    }

    public abstract V process(K key);
}
