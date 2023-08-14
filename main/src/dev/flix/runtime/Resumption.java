package dev.flix.runtime;

public interface Resumption {

    static Result rewind(Resumption k, /* type? */ int v) { // TODO: instance method.
        if (k instanceof ResumptionNil) {
            return Value.mkInt32(v);
        } else if (k instanceof ResumptionCons) {
            ResumptionCons cons = (ResumptionCons) k;
            return new Thunk() { // Return thunk to avoid increase the stack.
                public Result apply() {
                    return Handler.installHandler(cons.effSym, cons.handler, cons.frames, new Thunk() {
                        public Result apply() {
                            return rewind(cons.tail, v);
                        }
                    });
                }
            };
        } else {
            throw new RuntimeException("Impossible");
        }
    }

}
