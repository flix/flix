package flix.test;

import java.util.Arrays;

public interface TestInt32ArrayInterface {
  int[] testMethod(int[] xs);

  static boolean runTest(TestInt32ArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new int[] {1, 2, 3}), new int[] {2, 3, 4});
  }
}
