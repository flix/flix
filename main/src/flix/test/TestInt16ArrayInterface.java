package flix.test;

import java.util.Arrays;

public interface TestInt16ArrayInterface {
  short[] testMethod(short[] xs);

  static boolean runTest(TestInt16ArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new short[] {1, 2, 3}), new short[] {2, 3, 4});
  }
}
