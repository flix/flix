package ca.uwaterloo.flix.util.misc;

public class SampleLattice {

    public static String BOT = "<<bot>>";

    public static String TOP = "<<top>>";

    public static boolean leq(String e1, String e2) {
        return e1.equals(e2);
    }

    public static boolean testBool(boolean b1, boolean b2) {
        return b1 && b2;
    }

    public static String lub(String e1, String e2) {
        return "";
    }

    public static String glb(String e1, String e2) {
        return "";
    }
}
