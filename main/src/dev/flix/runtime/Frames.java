package dev.flix.runtime;

/**
 * Frames = FramesNil | FramesCons.
 */
public interface Frames {
    Frames push(Frame t);

    Frames reverseOnto(Frames rest);
}
