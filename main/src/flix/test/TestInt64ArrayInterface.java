package flix.test;

import java.util.Arrays;

public interface TestInt64ArrayInterface {
  long[] testMethod(long[] xs);

  static boolean runTest(TestInt64ArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new long[] {1, 2, 3}), new long[] {2, 3, 4});
  }
}
