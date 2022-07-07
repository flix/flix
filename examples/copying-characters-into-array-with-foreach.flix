// Easily iterate over arrays with foreach-loops.
def main(): Unit & Impure =
    use String.enumerator;
    use Array.length;

    region r { // Declare a new region.

        // Create a string we want to characters of.
        let s = "Flix is cool!";

        // Array to store 8 chars in.
        let xs = ['a'; 8] @ r;

        // Enumerate the chars of `s` along
        // with the index of the char.
        // The loop has a guard to avoid
        // out of bounds errors.
        foreach ((x, i) <- enumerator(r, s);
                 if i < length(xs))
                    xs[i] = x;

        println(xs)

    }
