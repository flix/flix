rel Compiler(src: Str, dst: Str, impl: Str)
rel Interpreter(lang: Str)

// C1: an “optimizing” compiler from Scala to x86, implemented in MiniScala
// C2: a “quick-and-dirty” compiler from MiniScala to C++, implemented in C++
// C3: a compiler from C++ to x86 implemented in x86
// I1: an x86 machine (= an interpreter for x86)
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
