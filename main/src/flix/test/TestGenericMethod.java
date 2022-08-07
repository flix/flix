package flix.test;

public interface TestGenericMethod {
  <T1> T1 testMethod(T1 x);

  static boolean runTest(TestGenericMethod obj) {
    return obj.testMethod("foo").equals("foo, foo");
  }
}
