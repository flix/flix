package dev.flix.runtime;

public class FramesCons implements Frames {
    public final Frame head;
    public final Frames tail;

    public FramesCons(Frame head, Frames tail) {
        this.head = head;
        this.tail = tail;
    }

    public Frames reverseOnto(Frames rest) {
        return tail.reverseOnto(new FramesCons(head, rest));
    }

}
