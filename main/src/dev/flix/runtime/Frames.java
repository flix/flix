package dev.flix.runtime;

interface Frames {
    default Frames push(Thunk t) {
        return new FramesCons(t, this);
    }
}
class FramesNil implements Frames {}

class FramesCons implements Frames {
    public final Thunk head;
    public final Frames tail;

    public FramesCons(Thunk head, Frames tail) {
        this.head = head;
        this.tail = tail;
    }
}
