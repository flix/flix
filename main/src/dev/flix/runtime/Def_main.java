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
        Result vResult = Def_u.apply(new Locals_u(0, null, null));
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
            if (s.effSym.equals("Con")) { // TODO: Should be a check that uses a Class object.
                UseOfConsole use = (UseOfConsole) s.effOp;
                return use.apply(handler); // TODO: Need to use suspensions.
            } else {
                // This is not the right handler.
                // We need to capture this frame... and then
                // return another suspension.

                // Package the effect handler up with the prefix.
                Resumption r = new ResumptionCons("Con", handler, s.prefix, s.resumption);
                return new Suspension(s.effSym, s.effOp, new FramesNil(), r);
                // Note: If we were not in tail position then we would put the FrameData/Locals of
                // main and its PC into a singleton list frames.
                // Because main is just calling u, and nothing more.

                // TODO: Need to remember the handlers (i.e. the bodies of the def read and def print).
            }
        }


        return null;
    }

}

class Locals_main {
}
