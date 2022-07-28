package flix.test;

public interface TestInt32Interface {
  int testMethod(int x);

  static boolean runTest(TestInt32Interface obj) {
    return obj.testMethod(42) == 43;
  }
}
