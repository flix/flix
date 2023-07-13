package dev.flix.runtime;

public interface Resumption {

    static Result rewind(Resumption k, /* type? */ int v) { // TODO: instance method.
        if (k instanceof ResumptionNil) {
            return Done.mkInt32(v);
        } else if (k instanceof ResumptionCons) {
            ResumptionCons cons = (ResumptionCons) k;
            return Handler.installHandler(cons.effSym, cons.handler, cons.frames, new Thunk() {
                public Result apply() {
                    return rewind(((ResumptionCons) k).tail, v);
                }
            });
        } else {
            throw new RuntimeException("Impossible");
        }
    }

}
