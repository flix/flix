package dev.flix.runtime;

public class Def_g implements Cont {
    public int result;

    public Action apply() {
        // TailCall, construct continuation, and simply return it.
        Def_h h = new Def_h();
        h.arg0 = 1;
        return h;
    }

}
