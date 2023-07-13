package dev.flix.runtime;

public class FramesCons implements Frames {
    public final Thunk head;
    public final Frames tail;

    public FramesCons(Thunk head, Frames tail) {
        this.head = head;
        this.tail = tail;
    }

    public Frames reverseOnto(Frames rest) {
        return tail.reverseOnto(new FramesCons(head, rest));
    }

}
