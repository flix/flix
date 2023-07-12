package dev.flix.runtime;

public class FramesCons implements Frames {
    public final Thunk head;
    public final Frames tail;

    public FramesCons(Thunk head, Frames tail) {
        this.head = head;
        this.tail = tail;
    }
}
