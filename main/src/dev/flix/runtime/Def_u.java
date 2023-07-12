package dev.flix.runtime;

public class Def_u {

    //    def u(): Int32 \ Con =
    //      let name = do Con.read();
    //      let greetings = "Hello ${name}";
    //      do Con.print(greetings);
    //      String.length(name)
    public FrameOrDone apply(Def_u_Suspend frame) {
        // locals
        String name;
        String greetings;

        while (true) {
            if (frame.pc == 0) { // invariant: frame is null.
                // no local variables to restore.
                return new Def_u_Suspend(1, "Con.read", null, null);
            } else if (frame.pc == 1) {
                // must restore name
                name = frame.name;
                greetings = "Hello " + name;
                return new Def_u_Suspend(2, "Con.print", name, greetings);
            } else if (frame.pc == 2) {
                // restore name and greetings
                name = frame.name;
                greetings = frame.greetings;
                return new Def_u_Done(greetings.length());
            }
        }

    }

}

interface FrameOrDone {
}

class Def_u_Done implements FrameOrDone {
    public int result;

    public Def_u_Done(int result) {
        this.result = result;
    }
}

class Def_u_Suspend implements FrameOrDone {
    public int pc;
    public String op;
    public String name;
    public String greetings;

    public Def_u_Suspend(int pc, String op, String name, String greetings) {
        this.pc = pc;
        this.op = op;
        this.name = name;
        this.greetings = greetings;
    }
}
