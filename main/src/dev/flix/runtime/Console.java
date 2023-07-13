package dev.flix.runtime;

public interface Console {
    Result read(Resumption k);

    Result print(String s, Resumption k);

}

class ConsoleHandler17 implements Console {

    // TODO: Move somewhere else.
    public static Result rewind(Resumption k, /* type? */ int v) {
        if (k instanceof ResumptionNil) {
            return Done.mkInt32(v);
        } else if (k instanceof ResumptionCons) {
            ResumptionCons cons = (ResumptionCons) k;
            return installHandler(cons.effSym, cons.handler, new Thunk() {
                public Result apply() {
                    return rewind(((ResumptionCons) k).tail, v);
                }
            });
        } else {
            throw new RuntimeException("Impossible");
        }
    }

    // TODO: Move somewhere else.
    public static Result installHandler(String effSym, Object handler, Thunk thunk) {

        return null;
    }

    public Result read(Resumption k) {
        return rewind(k, 42);
    }

    public Result print(String s, Resumption k) {
        return Done.mkInt32(42);
    }

}

// and then we can create objects/instances for each execution of the try-with.
// Each ResumptionCons contains a handler instance (typed as Object).
