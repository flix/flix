package dev.flix.runtime;

public class ResumptionCons implements Resumption {
    public final String effSym; // TODO: Could be classObject.
    public final Object handler; // TODO: Add marker trait.
    public final Frames frames;
    public final Resumption tail;
    public ResumptionCons(String effSym, Object handler, Frames frames, Resumption tail) {
        this.effSym = effSym;
        this.handler = handler;
        this.frames = frames;
        this.tail = tail;
    }

}
