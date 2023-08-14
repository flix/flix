package dev.flix.runtime;

/**
 * Frames = FramesNil | FramesCons.
 *
 *
 */
public interface Frames {
    default Frames push(Frame t) {
        return new FramesCons(t, this);
    }

    // TODO: Implement other reverse function.
    default Frames reverse() {
        return reverseOnto(new FramesNil());
    }
    Frames reverseOnto(Frames rest);
}
