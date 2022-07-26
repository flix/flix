package flix.test;

import java.util.Arrays;

public interface TestCharArrayInterface {
  char[] testMethod(char[] xs);

  static boolean runTest(TestCharArrayInterface obj) {
    return Arrays.equals(obj.testMethod(new char[] {'a', 'b', 'c'}), new char[] {'A', 'B', 'C'});
  }
}
