package dev.flix.runtime;

public class ResumptionCons implements Resumption {
    public final String sym; // TODO: Could be classObject.
    public final Handler handler;
    public final Frames frames;
    public final Resumption tail;

    public ResumptionCons(String sym, Handler handler, Frames frames, Resumption tail) {
        this.sym = sym;
        this.handler = handler;
        this.frames = frames;
        this.tail = tail;
    }

    @Override
    public Result rewind(Value v) {
        return new Thunk() { // Return thunk to avoid increase the stack.
            @Override
            public Result invoke() {
                return Handler.installHandler(sym, handler, frames, new Thunk() {
                    @Override
                    public Result invoke() {
                        return Resumption.staticRewind(tail, v);
                    }
                });
            }
        };
    }

}
