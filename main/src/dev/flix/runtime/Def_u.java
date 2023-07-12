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

        jump: while(true) {
            switch (pc) {
                case 0:
                    Def_v v = new Def_v();
                    //v.arg = Unit

                    Result vResult = v.apply();
                    // -- below can be put into Unwind which returns only Null + Suspend
                    if (vResult instanceof Thunk) {

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

                    if (vResult == null) {
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
                    var kont = new EmptyKont();
                    kont = kont.push(this, new FrameData_u(2, name, greetings));
                    return new Suspend("Con.print", greetings, kont);
                    break;

                case 2:
                    this.result = name.length();
                    return null;
            }
        }

    }
}

class FrameData_u implements Entrypoint {
    public int pc;

    public String name;
    public String greetings;

    public FrameData_u(int pc, String name, String greetings) {
        this.pc = pc;
        this.name = name;
        this.greetings = greetings;
    }
}
