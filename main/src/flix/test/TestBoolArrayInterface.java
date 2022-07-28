package flix.test;

import java.util.Arrays;

public interface TestBoolArrayInterface {
  boolean[] testMethod(boolean[] xs);

  static boolean runTest(TestBoolArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new boolean[] {true, false, true}), new boolean[] {false, true, false});
  }
}
