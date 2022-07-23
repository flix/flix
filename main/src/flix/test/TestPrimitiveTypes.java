package flix.test;

public interface TestPrimitiveTypes {
  void takesAndReturnsVoid();
  boolean takesAndReturnsBoolean(boolean x);
  char takesAndReturnsChar(char x);
  byte takesAndReturnsByte(byte x);
  short takesAndReturnsShort(short x);
  int takesAndReturnsInt(int x);
  long takesAndReturnsLong(long x);
  float takesAndReturnsFloat(float x);
  double takesAndReturnsDouble(double x);

  // String allTheTypes(boolean a, char b, byte c, short d, int e, long f, float g, double h);

  static boolean runTest(TestPrimitiveTypes obj) {
    obj.takesAndReturnsVoid();
    return obj.takesAndReturnsBoolean(false) &&
      obj.takesAndReturnsChar('a') == 'A' &&
      obj.takesAndReturnsByte((byte)1) == (byte)2 &&
      obj.takesAndReturnsShort((short)1) == (short)2 &&
      obj.takesAndReturnsInt(1) == 2 &&
      obj.takesAndReturnsLong(1) == 2 &&
      obj.takesAndReturnsFloat(1.0f) == 2.23f &&
      obj.takesAndReturnsDouble(1.0d) == 2.23d;
      // obj.allTheTypes(true, 'a', (byte)1, (short)2, 3, 4, 5.6f, 7.8d) == "";
  }
}