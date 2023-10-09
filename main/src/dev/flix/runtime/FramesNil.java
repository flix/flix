package dev.flix.runtime;

public class FramesNil implements Frames {
    public Frames push(Frame t) {
        return new FramesCons(t, this);
    }

    public Frames reverseOnto(Frames rest) {
        return rest;
    }
}

