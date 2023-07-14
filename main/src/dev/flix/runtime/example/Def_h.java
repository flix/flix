package dev.flix.runtime.example;

import dev.flix.runtime.Result;
import dev.flix.runtime.Value;

public class Def_h implements Result {

    public static Result invoke() {
        return Value.mkInt32(1); // TODO: Should be x + 1, need locals as argument.
    }

}
