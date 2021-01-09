/// We can use first-class Datalog constraints to solve the following
/// complex reachability problem: Given a collection of compilers and
/// interpreters, what source languages can be compiled to what targets?
def main(_args: Array[String]): Int32 & Impure =
    let p = #{
    /// We have the following interpreters and compilers:
    Interpreter("x86").
    Compiler("Scala", "x86", "MiniScala").
    Compiler("MiniScala", "C++", "C++").
    Compiler("C++", "x86", "x86").

    // Bootstrapping Compilation:
    // We have a compiler from src1 -> dst1 written in lang1.
    // We have a compiler that can compile lang1 to dst2.
    // Now we have a compiler from src1 to dst1 written in dst2.
    Compiler(src1, dst1, dst2) :-
        Compiler(src1, dst1, lang1),
        Compiler(lang1, dst2, lang2),
        Interpreter(lang2).

    // Transitive Compilation:
    // If we have a compiler from src -> intermediate and
    // we have a compiler from intermediate -> dst then
    // we can obtain a compiler from src -> dst.
    Compiler(src, dst, lang) :-
        Compiler(src, intermediate, lang),
        Compiler(intermediate, dst, lang),
        Interpreter(lang).
    };
    (solve p) |> unsafeToString |> println;
    0 // exit code
