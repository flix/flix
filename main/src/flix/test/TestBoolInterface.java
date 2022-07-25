package flix.test;

public interface TestBoolInterface {
  boolean testMethod(boolean x);

  static boolean runTest(TestBoolInterface obj) {
    return obj.testMethod(false) == true;
  }
}
