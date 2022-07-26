package flix.test;

import java.util.Arrays;

public interface TestInt8ArrayInterface {
  byte[] testMethod(byte[] xs);

  static boolean runTest(TestInt8ArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new byte[] {1, 2, 3}), new byte[] {2, 3, 4});
  }
}
