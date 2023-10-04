package dev.flix.runtime;

public class FramesCons implements Frames {
    public Frame head;
    public Frames tail;

    public FramesCons(Frame head, Frames tail) {
        this.head = head;
        this.tail = tail;
    }

    public Frames push(Frame t) {
        return new FramesCons(t, this);
    }

    public Frames reverseOnto(Frames rest) {
        return tail.reverseOnto(new FramesCons(head, rest));
    }

}
