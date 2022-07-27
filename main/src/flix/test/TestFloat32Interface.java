package flix.test;

public interface TestFloat32Interface {
  float testMethod(float x);

  static boolean runTest(TestFloat32Interface obj) {
    return obj.testMethod(1.0f) == 1.23f;
  }
}
