package dev.flix.runtime;

public interface Resumption {

    static Result rewind(Resumption k, Value v) { // TODO: instance method.
        if (k instanceof ResumptionNil) {
            return v;
        } else if (k instanceof ResumptionCons) {
            ResumptionCons cons = (ResumptionCons) k;
            return new Thunk() { // Return thunk to avoid increase the stack.
                @Override
                public Result invoke() {
                    return Handler.installHandler(cons.effSym, cons.handler, cons.frames, new Thunk() {
                        @Override
                        public Result invoke() {
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
