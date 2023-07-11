package dev.flix.runtime;

public class Def_f {

    private int result;

    public int getResult() {
        return this.result;
    }

    public Action apply() {
        // Non-tail call. Must unwind the continuations.
        Def_g g = new Def_g();

        Action prev = null;
        Action curr = g;

        do {
            if (curr instanceof Cont) {
                Cont c = (Cont) curr;
                prev = curr;
                curr = c.apply();
            }

        } while (curr != null);

        result = prev.getResult();

        return null; // We are done.
    }

}
