package dev.flix.runtime;

public interface Frames {
    default Frames push(Thunk t) {
        return new FramesCons(t, this);
    }
}
