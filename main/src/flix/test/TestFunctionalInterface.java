package flix.test;

@FunctionalInterface
public interface TestFunctionalInterface {
  int testMethod(int x);

  static boolean runTest(TestFunctionalInterface obj) {
    return obj.testMethod(1) == 2;
  }
}
