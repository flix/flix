package flix.test;

public interface TestGenericInterface<T1> {
  T1 testMethod(T1 x);

  static boolean runTest(TestGenericInterface<String> obj) {
    return obj.testMethod("foo").equals("foo, foo");
  }
}
