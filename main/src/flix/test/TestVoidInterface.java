package flix.test;

public interface TestVoidInterface {
  void testMethod();

  static boolean runTest(TestVoidInterface obj) {
    obj.testMethod();
    return true;
  }
}
