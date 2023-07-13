package dev.flix.runtime;

import dev.flix.runtime.example.EffectCall;

public interface Console {
    Result read(Resumption k);

    Result print(String s, Resumption k);

}

class ConsoleHandler17 implements Console, Handler {

    // TODO: Move somewhere else.
    public static Result rewind(Resumption k, /* type? */ int v) {
        if (k instanceof ResumptionNil) {
            return Done.mkInt32(v);
        } else if (k instanceof ResumptionCons) {
            ResumptionCons cons = (ResumptionCons) k;
            return installHandler(cons.effSym, cons.handler, cons.frames, new Thunk() {
                public Result apply() {
                    return rewind(((ResumptionCons) k).tail, v);
                }
            });
        } else {
            throw new RuntimeException("Impossible");
        }
    }

    // TODO: Move somewhere else.
    public static Result installHandler(String effSym, Handler handler, Frames frames, Thunk thunk) {
        Result vResult = thunk.apply();
        while (vResult instanceof Thunk) {
            vResult = ((Thunk) vResult).apply();
        }
        // Now two cases:
        if (vResult instanceof Suspension) {
            Suspension s = (Suspension) vResult;
            Frames newFrames = s.prefix.reverseOnto(frames);
            Resumption r = new ResumptionCons(effSym, handler, newFrames, s.resumption);

            if (s.effSym.equals(effSym)) {
                EffectCall use = (EffectCall) s.effOp;
                return use.apply(handler, r);
            } else {
                return new Suspension(s.effSym, s.effOp, new FramesNil(), r);
            }
        } else if (vResult instanceof Done) {
            Done res = (Done) vResult;

            if (frames instanceof FramesNil) {
                return vResult; // We are completely done!
                // This means that we are returning through the handler and drop the handler.
            } else if (frames instanceof FramesCons) {
                FramesCons cons = ((FramesCons) frames);
                return installHandler(effSym, handler, cons.tail, new Thunk() {
                    public Result apply() {
                        return cons.head.apply(res);
                    }
                });
            }
        }
        throw new RuntimeException("Impossible");
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
