package flix.test;

public interface TestGenerics<T1> {
  T1 methodOne(T1 x);
  <T2> T1 methodTwo(T1 x, T2 y);

  static boolean runTest(TestGenerics<String> obj) {
    return obj.methodOne("foo").equals("foo, foo") &&
      obj.methodTwo("foo", Integer.valueOf(2)).equals("foo, 2");
  }
}
