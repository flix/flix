package dev.flix.runtime;

public class Def_h implements Action {

    public int arg0;
    public int result;

    public Action apply() {
        result = this.arg0 + 1; // Compute the result.
        return null;            // We are done, return null.
    }

}
