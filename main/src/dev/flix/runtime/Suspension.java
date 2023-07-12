package dev.flix.runtime;

public class Suspension implements Result {
    public final String effSym;
    public final String effOp;
    public final Object effArg;
    public final Frames prefix;
    public final Resumption resumption;

    public Suspension(String effSym, String effOp, Object effArg, Frames prefix, Resumption resumption) {
        this.effSym = effSym;
        this.effOp = effOp;
        this.effArg = effArg;
        this.prefix = prefix;
        this.resumption = resumption;
    }
}
