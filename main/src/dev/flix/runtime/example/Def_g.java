package dev.flix.runtime.example;

import dev.flix.runtime.Result;
import dev.flix.runtime.Thunk;

public class Def_g implements Thunk {
    private int result;

    public int getResult() {
        return this.result;
    }

    public Result apply() {
        // TailCall, construct continuation, and simply return it.
        Def_h h = new Def_h();
        h.arg0 = 1;
        return h;
    }

}
