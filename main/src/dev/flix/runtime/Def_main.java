package dev.flix.runtime;

import dev.flix.runtime.example.Def_u;
import dev.flix.runtime.example.Locals_u;
import dev.flix.runtime.example.UseOfConsole;

public class Def_main {

    public static void main(String[] args) {
        Result vResult = apply(null);
        while (vResult instanceof Thunk) {
            vResult = ((Thunk) vResult).apply();
        }
        if (vResult instanceof Suspension) {
            throw new RuntimeException("Unhandled effect");
        }

        System.out.println(vResult);
    }

    public static Result apply(Locals_main locals) {
        // restore locals
        Result vResult = Def_u.apply(new Locals_u(0, null, null), null);
        while (vResult instanceof Thunk) {
            vResult = ((Thunk) vResult).apply();
        }

        // Now vResult is Done or Suspension
        if (vResult instanceof Done) {
            return vResult;
        }

        ConsoleHandler17 handler = new ConsoleHandler17();

        // Now vResult is a suspension? Do we have a handler for this specific effect
        if (vResult instanceof Suspension) {
            Suspension s = (Suspension) vResult;
            // Package the effect handler up with the prefix.
            Resumption r = new ResumptionCons("Con", handler, s.prefix.reverse(), s.resumption);

            if (s.effSym.equals("Con")) { // TODO: Should be a check that uses a Class object.
                UseOfConsole use = (UseOfConsole) s.effOp;
                return use.apply(handler, r);
            } else {
                // This is not the right handler.
                // We need to capture this frame... and then
                // return another suspension.

                return new Suspension(s.effSym, s.effOp, new FramesNil(), r);
                // Note: If we were not in tail position then we would put the FrameData/Locals of
                // main and its PC into a singleton list frames.
                // Because main is just calling u, and nothing more.
            }
        }


        return null;
    }

}

class Locals_main {
}
