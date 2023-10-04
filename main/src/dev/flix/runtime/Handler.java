package dev.flix.runtime;

public interface Handler {
    static Result installHandler(String effSym, Handler handler, Frames frames, Thunk thunk) {
        Result vResult = thunk.invoke();
        while (vResult instanceof Thunk) {
            vResult = ((Thunk) vResult).invoke();
        }
        // Now two cases:
        if (vResult instanceof Suspension) {
            Suspension s = (Suspension) vResult;
            Frames newFrames = s.prefix.reverseOnto(frames);
            Resumption r = new ResumptionCons(effSym, handler, newFrames, s.resumption);

            if (s.effSym.equals(effSym)) {
                // found the right handler, apply the effect operation.
                EffectCall use = s.effOp;
                return use.apply(handler, r);
            } else {
                // forward, not right handler.
                return new Suspension(s.effSym, s.effOp, new FramesNil(), r);
            }
        } else if (vResult instanceof Value) {
            Value res = (Value) vResult;

            if (frames instanceof FramesNil) {
                return vResult; // We are completely done!
                // This means that we are returning through the handler and drop the handler.
            } else if (frames instanceof FramesCons) {
                FramesCons cons = ((FramesCons) frames);
                return installHandler(effSym, handler, cons.tail, new Thunk() {
                    @Override
                    public Result invoke() {
                        return cons.head.apply(res);
                    }
                });
            }
        }
        throw new RuntimeException("Impossible");
    }
}
