package flix.test;

import java.util.Arrays;

public interface TestFloat64ArrayInterface {
  double[] testMethod(double[] xs);

  static boolean runTest(TestFloat64ArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new double[] {1.0d, 2.0d, 3.0d}), new double[] {1.5d, 2.5d, 3.5d});
  }
}
