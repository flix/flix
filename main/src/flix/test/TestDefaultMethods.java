package flix.test;

public interface TestDefaultMethods {
  int methodWithNoImplementation(int x);

  default int methodWithDefaultImplementation(int x) {
    return x + 42;
  }
}
