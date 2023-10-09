package dev.flix.runtime;

public class ResumptionCons implements Resumption {
    public final String effSym; // TODO: Could be classObject.
    public final Handler handler;
    public final Frames frames;
    public final Resumption tail;

    public ResumptionCons(String effSym, Handler handler, Frames frames, Resumption tail) {
        this.effSym = effSym;
        this.handler = handler;
        this.frames = frames;
        this.tail = tail;
    }

    @Override
    public Result rewind(Value v) {
        return new Thunk() { // Return thunk to avoid increase the stack.
            @Override
            public Result invoke() {
                return Handler.installHandler(effSym, handler, frames, new Thunk() {
                    @Override
                    public Result invoke() {
                        return tail.rewind(v);
                    }
                });
            }
        };
    }

}
