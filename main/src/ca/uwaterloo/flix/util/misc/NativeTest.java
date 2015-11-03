package ca.uwaterloo.flix.util.misc;

// See examples/misc/NativeTest.flix for the program that uses this native class.
public class NativeTest {

    public static boolean TRUE = true;
    public static boolean FALSE = false;

    public static boolean not1(boolean b) { return !b; }
    public static boolean not2(Boolean b) { return !b; }
    public static Boolean not3(boolean b) { return !b; }
    public static Boolean not4(Boolean b) { return !b; }

    public static int primitiveInt = 1;
    public static Integer boxedInt = 1;

    public static int increment1(int i) { return i + 1; }
    public static int increment2(Integer i) { return i + 1; }
    public static Integer increment3(int i) { return i + 1; }
    public static Integer increment4(Integer i) { return i + 1; }

    public static String hello = "hello";
    public static String world = "world";

    public static boolean streq(String s1, String s2) {
        return s1.equals(s2);
    }

    public static String strcat(String s1, String s2) {
        return s1 + s2;
    }
}
