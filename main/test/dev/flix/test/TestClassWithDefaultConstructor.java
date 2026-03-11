package dev.flix.test;

abstract public class TestClassWithDefaultConstructor {
  public int m_x;
  public String m_y;

  public TestClassWithDefaultConstructor() {
    m_x = 42;
    m_y = "foo";
  }

  public abstract int abstractMethod(int x);
  public String concreteMethod(String y) {
    return m_y + y;
  }

  public static int staticMethod(int x) {
    return x + 1;
  }

  public static int staticField = 123;
}
