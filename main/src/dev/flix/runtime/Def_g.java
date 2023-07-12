package dev.flix.runtime;

public class Def_g implements Thunk {
    private int result;

    public int getResult() {
        return this.result;
    }

    public Action apply() {
        // TailCall, construct continuation, and simply return it.
        Def_h h = new Def_h();
        h.arg0 = 1;
        return h;
    }

}
