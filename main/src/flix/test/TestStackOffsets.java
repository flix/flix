package flix.test;

public interface TestStackOffsets {
  String testMethod(boolean a, char b, byte c, short d, int e, long f, float g, double h);

  static boolean runTest(TestStackOffsets obj) {
    return obj.testMethod(true, 'a', (byte)1, (short)2, 3, 4, 5.6f, 7.8d).equals("true, a, 1, 2, 3, 4, 5.6, 7.8");
  }
}