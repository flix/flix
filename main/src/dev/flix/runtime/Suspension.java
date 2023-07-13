package dev.flix.runtime;

public class Suspension implements Result {
    public final String effSym;
    public final Object effOp;
    public final Frames prefix; // Note: Frames are in wrong order, but they have to be reversed.
    public final Resumption resumption;

    public Suspension(String effSym, Object effOp, Frames prefix, Resumption resumption) {
        this.effSym = effSym;
        this.effOp = effOp;
        this.prefix = prefix;
        this.resumption = resumption;
    }
}
