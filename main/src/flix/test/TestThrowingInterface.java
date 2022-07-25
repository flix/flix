package flix.test;

public interface TestThrowingInterface {
  void testMethod() throws UnsupportedOperationException;

  public static boolean runTest(TestThrowingInterface obj) {
    obj.testMethod();
    return true;
  }
}
