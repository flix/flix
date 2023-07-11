package dev.flix.runtime;

public class Def_f {

    public int result;

    public Cont apply() {
        // Non-tail call. Must unwind the continuations.
        Def_g g = new Def_g();

        Cont prev = null;
        Cont curr = g;

        do {
            if (curr instanceof TailCall) {
                TailCall c = (TailCall) curr;
                prev = curr;
                curr = c.apply();
            }

        } while (curr != null);

        return null; // TODO: Update the result.
    }

}
