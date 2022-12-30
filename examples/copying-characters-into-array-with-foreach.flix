// Easily iterate over arrays with foreach-loops.
def main(): Unit \ IO = region r {

        // Create a string to separate characters of.
        let s = "Flix is cool!";

        // Array to store 8 chars in.
        let xs = Array.repeat(r, 8, 'a');

        // Print contents of `xs`.
        foreach (x <- xs)
            println(x);

        // Enumerate the chars of `s` along
        // with the index of the char.
        // The loop has a guard to avoid
        // out of bounds errors.
        // It also matches the pattern of the
        // value returned by `enumerator.`
        foreach ((i, x) <- String.enumerator(r, s);
                 if i < Array.length(xs))
                    xs[i] = x;

        println(xs)
}
