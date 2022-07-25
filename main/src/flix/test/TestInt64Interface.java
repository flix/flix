package flix.test;

public interface TestInt64Interface {
  long testMethod(long x);

  static boolean runTest(TestInt64Interface obj) {
    return obj.testMethod(42) == 43;
  }
}
