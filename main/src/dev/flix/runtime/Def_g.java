package dev.flix.runtime;

public class Def_g implements TailCall {
    public int result;

    public Cont apply() {
        // TailCall, construct continuation, and simply return it.
        Def_h h = new Def_h();
        h.arg0 = 1;
        return h;
    }

}
