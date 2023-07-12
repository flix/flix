package dev.flix.runtime.example;

import dev.flix.runtime.Result;

public class Def_h implements Result {

    public int arg0;
    private int result;

    public int getResult() {
        return this.result;
    }

    public Result apply() {
        result = this.arg0 + 1; // Compute the result.
        return null;            // We are done, return null.
    }

}
