package flix.test;

public interface TestPrimitiveTypes {
  void takesAndReturnsVoid();
  int takesAndReturnsInt(int x);

  static boolean runTest(TestPrimitiveTypes obj) {
    obj.takesAndReturnsVoid();
    return obj.takesAndReturnsInt(1) == 2;
  }
}