package flix.test;

public interface TestCharInterface {
  char testMethod(char x);

  static boolean runTest(TestCharInterface obj) {
    return obj.testMethod('a') == 'A';
  }
}
