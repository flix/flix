package dev.flix.runtime;

public class ResumptionCons implements Resumption {
    public final String effSym;
    public final Frames frames;
    public final Resumption tail;
// TODO: Something about handlers. To be decided.
    public ResumptionCons(String effSym, Frames frames, Resumption tail) {
        this.effSym = effSym;
        this.frames = frames;
        this.tail = tail;
    }

}
