package dev.flix.test;

/**
 * A test class with constructors that require arguments.
 * Used to test user-defined JvmConstructor in NewObject expressions.
 */
abstract public class TestClassWithArgConstructor {
    public int m_x;
    public String m_y;

    public TestClassWithArgConstructor(int x) {
        m_x = x;
        m_y = "default";
    }

    public TestClassWithArgConstructor(int x, String y) {
        m_x = x;
        m_y = y;
    }

    public abstract int abstractMethod(int n);
}
