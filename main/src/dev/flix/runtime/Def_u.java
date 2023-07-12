package dev.flix.runtime;

public class Def_u implements Result {

    int result;

    public int getResult() {
        return result;
    }

    //    def u(): Int32 \ Con =
    //      let name = v();
    //      let greetings = "Hello ${name}";
    //      do Con.print(greetings);
    //       String.length("${name}")

    public Result apply(FrameData_u fd) {

        int pc = fd.pc;
        String name = fd.name;
        String greetings = fd.greetings;
        // actually the PC is mapped to a switch that jumps to the appropriate label,
        // i.e. we have a mapping from PCs to labels.
        // The PCs and labels come from Stmt.LetVal... (?)
        jump: while(true) {
            switch (pc) {
                case 0:
                    Def_v v = new Def_v();
                    //v.arg = Unit

                    Result vResult = v.apply();
                    // -- below can be put into Unwind which returns only Null + Suspend
                    if (vResult instanceof Thunk) {

                        // TODO: Simplify and use Done!
                        Result curr = vResult;
                        Result prev = curr;
                        do {
                            prev = curr;
                            curr = ((Thunk) curr).apply();
                        } while (curr != null && curr instanceof Thunk);
                        vResult = prev;
                    }
                    // --

                    // Now vResult cannot be Thunk, it can only be: Null or Suspend.

                    if (vResult == null) { // TODO: Not right
                        name = vResult.getResult(); // Thunk for String
                        pc = 1;
                        continue jump;
                    } else if (vResult instanceof Suspend) {
                        // Build frame, and then return new suspension.
                        Suspend s = (Suspend) vResult;
                        return s.push(this, new FrameData_u(1, name, greetings));
                    } else { /* impossible: we have already dealt with all the thunks. */ }

                    break;

                case 1:
                    greetings = "Hello " + name;
                    var prefix0 = new FramesNil();
                    var prefix = prefix0.push(new Def_u_Frame( new FrameData_u(2, name, greetings)));
                    return new Suspend("Con", "print", greetings, prefix, new ResumptionNil());
                    break;

                case 2:
                    this.result = name.length();
                    return null;
            }
        }

    }
}

class Def_u_Frame implements Thunk {
    FrameData_u frameData;
    Def_u defu = new Def_u();

    public Def_u_Frame(FrameData_u frameData) {
        this.frameData = frameData;
    }

    public int getResult() {
        return defu.result;
    }

    public Result apply() {
        return defu.apply(frameData);
    }
}

interface Frames {
    default Frames push(Thunk t) {
        return new FramesCons(t, this);
    }
}
class FramesNil implements Frames {}
class FramesCons implements Frames {
    final Thunk head;
    final Frames tail;
    public FramesCons(Thunk head, Frames tail) {
        this.head = head;
        this.tail = tail;
    }
}


interface Resumption {}
class ResumptionNil implements Resumption {}
class ResumptionCons implements Resumption {
    final String effName;
    final Frames frames;
    final Resumption tail;

    public ResumptionCons(String effName, Frames frames, Resumption tail) {
        this.effName = effName;
        this.frames = frames;
        this.tail = tail;
    }
}

class Suspend implements Result {
    final String effName;
    final String op;
    final Object opArgument;


    final Frames prefix;
    final Resumption resumption;

    public Suspend(String effName, String op, Object opArgument, Frames prefix, Resumption resumption) {
        this.effName = effName;
        this.op = op;
        this.opArgument = opArgument;
        this.prefix = prefix;
        this.resumption = resumption;
    }

    public int getResult() {
       throw null;
    }

}

class FrameData_u {
    public int pc;

    public String name;
    public String greetings;

    public FrameData_u(int pc, String name, String greetings) {
        this.pc = pc;
        this.name = name;
        this.greetings = greetings;
    }
}
