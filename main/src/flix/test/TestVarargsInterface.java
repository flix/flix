package flix.test;

public interface TestVarargsInterface {
  int testMethod(int... xs);

  static boolean runTest(TestVarargsInterface obj) {
    return obj.testMethod(1, 2, 3) == 6;
  }
}
