package flix.test;

abstract public class TestClassWithDefaultConstructor {
  int m_x;
  String m_y;

  public TestClassWithDefaultConstructor() {
    m_x = 42;
    m_y = "foo";
  }

  public abstract int abstractMethod(int x);
  public String concreteMethod(String y) {
    return m_y + y;
  }
}
