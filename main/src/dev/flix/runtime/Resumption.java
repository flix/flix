package dev.flix.runtime;

public interface Resumption {

    Result rewind(Value v);

    static Result staticRewind(Resumption r, Value v) {
        return r.rewind(v);
    }

}
