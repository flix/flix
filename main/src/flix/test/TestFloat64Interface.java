package flix.test;

public interface TestFloat64Interface {
  double testMethod(double x);

  static boolean runTest(TestFloat64Interface obj) {
    return obj.testMethod(1.0d) == 1.23d;
  }
}
