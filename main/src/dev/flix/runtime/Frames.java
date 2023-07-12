package dev.flix.runtime;

/**
 * Represents a list of frames (i.e. thunks).
 *
 * Frames = FramesNil | FramesCons(Thunk, Frames).
 */
interface Frames {
    default Frames push(Thunk t) {
        return new FramesCons(t, this);
    }
}
class FramesNil implements Frames {}

record FramesCons(Thunk head, Frames tail) implements Frames {} // TODO: Expand
