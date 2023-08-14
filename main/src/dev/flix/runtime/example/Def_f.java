package dev.flix.runtime.example;

import dev.flix.runtime.Result;
import dev.flix.runtime.Thunk;
import dev.flix.runtime.Value;

public class Def_f {

    public static Result invoke() { // TODO: need locals
        Result result = Def_h.invoke(); // TODO: Pass arguments
        while (result instanceof Thunk) {
            result = ((Thunk) result).apply();
        }

        var v = (Value) result;
        // TODO: No suspension here.
        return Value.mkInt32(v.int32 + 1);
    }

}
