package flix.test;

public interface TestInt16Interface {
  short testMethod(short x);

  static boolean runTest(TestInt16Interface obj) {
    return obj.testMethod((short)42) == 43;
  }
}
