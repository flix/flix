package flix.test;

import java.util.Arrays;

public interface TestFloat32ArrayInterface {
  float[] testMethod(float[] xs);

  static boolean runTest(TestFloat32ArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new float[] {1.0f, 2.0f, 3.0f}), new float[] {1.5f, 2.5f, 3.5f});
  }
}
