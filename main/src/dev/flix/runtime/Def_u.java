package dev.flix.runtime;

public class Def_u implements Action {

    @Override
    public int getResult() {
        return 0;
    }

//    def u(): Int32 \ Con =
//      let name = do Con.read();
//      let greetings = "Hello ${name}";
//      do Con.print(greetings);
//      String.length(greetings)
    public Action apply() {
        // Switch on PC.
        int pc = 0;

        if (pc == 0) {
            Def_u_suspend action = new Def_u_suspend();
            return action
        }

        return null;
    }

}

class Def_u_suspend implements Action {
    public String op() {
        return "Con.read"
    }
    public int getResult() {
        return 0;
    }
}
