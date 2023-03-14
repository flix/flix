// Easily iterate over arrays with foreach-loops.
def main(): Unit \ IO = region r {

        // Create a string to separate characters of.
        let s = "Flix is cool!";

        // Array to store 8 chars in.
        let xs = Array.repeat(r, 8, 'a');

        // Print contents of `xs`.
        Array.forEach(println, xs);

        ///
        /// Enumerator loops are not currently supported
        /// in Flix. Supported for them will be revived
        /// when `foreach` loops desugar to Streams
        /// rather than Iterators.
        ///
        // ~~~Enumerate the chars of `s` along
        // with the index of the char.
        // The loop has a guard to avoid
        // out of bounds errors.
        // It also matches the pattern of the
        // value returned by `enumerator.`~~~
        /// foreach ((x, i) <- String.enumerator(r, s);
        ///          if i < Array.length(xs))
        ///             xs[i] = x;

        /// Enumerator loops can currently be simulated
        /// with `forEach` or `forEachWithIndex`.
        Iterator.forEach(match (i, x) -> {
            if (i < Array.length(xs))
                Array.put(x, i, xs)
            else
                ()
            }, String.enumerator(r, s));
        println(Array.toString(xs));

        Iterator.forEachWithIndex((i, x) -> {
            if (i < Array.length(xs))
                Array.put(x, i, xs)
            else
                ()
            }, String.iterator(r, s));
        println(Array.toString(xs))


}
