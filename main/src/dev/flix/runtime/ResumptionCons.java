package dev.flix.runtime;

public class ResumptionCons implements Resumption {
    public final String effSym;
    public final Frames frames;
    public final Resumption tail;

    public ResumptionCons(String effSym, Frames frames, Resumption tail) {
        this.effSym = effSym;
        this.frames = frames;
        this.tail = tail;
    }

}
