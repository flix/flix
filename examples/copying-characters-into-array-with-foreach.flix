// Easily iterate over arrays with foreach-loops.
def main(): Unit & Impure = region r {

        // Create a string to separate characters of.
        let s = "Flix is cool!";

        // Array to store 8 chars in.
        let xs = ['a'; 8] @ r;

        // Print contents of `xs`.
        foreach (x <- Array.iterator(r, xs))
            println(x);

        // Enumerate the chars of `s` along
        // with the index of the char.
        // The loop has a guard to avoid
        // out of bounds errors.
        // It also matches the pattern of the
        // value returned by `enumerator.`
        foreach ((x, i) <- String.enumerator(r, s);
                 if i < Array.length(xs))
                    xs[i] = x;

        println(xs)
}
