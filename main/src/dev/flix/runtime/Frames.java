package dev.flix.runtime;

public interface Frames {
    default Frames reverse() {
        return reverseOnto(new FramesNil());
    }
    Frames reverseOnto(Frames rest);
    default Frames push(Frame t) {
        return new FramesCons(t, this);
    }
}
