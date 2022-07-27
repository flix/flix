package flix.test;

public interface TestOverloadedMethods {
  int overloadedMethod();
  int overloadedMethod(int x);
  String overloadedMethod(String x, double y, double z);

  static boolean runTest(TestOverloadedMethods obj) {
    return obj.overloadedMethod() == 42 &&
      obj.overloadedMethod(1) == 2 &&
      obj.overloadedMethod("divided: ", 10.0d, 2.5d).equals("divided: 4.0");
  }
}
