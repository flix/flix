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

}
