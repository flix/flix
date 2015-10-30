package ca.uwaterloo.flix.util.misc;

public class NativeParity {

    public enum Parity {
                TOP,
        ODD,            EVEN,
                BOT
    }

    private Parity parity;
    private NativeParity(Parity e) {
        parity = e;
    }

    public String toString() {
        return "NativeParity(" + parity + ")";
    }

    public static NativeParity TOP = new NativeParity(Parity.TOP);
    public static NativeParity ODD = new NativeParity(Parity.ODD);
    public static NativeParity EVEN = new NativeParity(Parity.EVEN);
    public static NativeParity BOT = new NativeParity(Parity.BOT);

    public static boolean leq(NativeParity e1, NativeParity e2) {
        return e1 == BOT || e2 == TOP || e1 == e2;
    }

    public static NativeParity lub(NativeParity e1, NativeParity e2) {
        if (e1 == BOT) {
            return e2;
        } else if (e2 == BOT) {
            return e1;
        } else if (e1 == e2) {
            return e1;
        } else {
            return TOP;
        }
    }

    public static NativeParity glb(NativeParity e1, NativeParity e2) {
        if (e1 == TOP) {
            return e2;
        } else if (e2 == TOP) {
            return e1;
        } else if (e1 == e2) {
            return e1;
        } else {
            return BOT;
        }
    }

    public static NativeParity plus(NativeParity e1, NativeParity e2) {
        if (e1 == BOT || e2 == BOT) {
            return BOT;
        } else if (e1 == TOP || e2 == TOP) {
            return TOP;
        } else if (e1 == e2) {
            return EVEN;
        } else {
            return ODD;
        }
    }

    public static NativeParity times(NativeParity e1, NativeParity e2) {
        if (e1 == BOT || e2 == BOT) {
            return BOT;
        } else if (e1 == EVEN || e2 == EVEN) {
            return EVEN;
        } else if (e1 == ODD && e2 == ODD) {
            return ODD;
        } else {
            return TOP;
        }
    }

    public static boolean isMaybeZero(NativeParity e) {
        return e == EVEN || e == TOP;
    }

    public static boolean isMaybeOdd(NativeParity e) {
        return e == ODD || e == TOP;
    }
}
