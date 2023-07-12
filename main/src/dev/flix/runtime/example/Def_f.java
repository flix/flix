package dev.flix.runtime.example;

import dev.flix.runtime.Result;
import dev.flix.runtime.Thunk;

public class Def_f {

    private int result;

    public int getResult() {
        return this.result;
    }

    public Result apply() {
        // Non-tail call. Must unwind the continuations.
        Def_g g = new Def_g();

        Result prev = null;
        Result curr = g;

        do {
            if (curr instanceof Thunk) {
                Thunk c = (Thunk) curr;
                prev = curr;
                curr = c.apply();
            }

        } while (curr != null);

        result = prev.getResult();

        return null; // We are done.
    }

}
