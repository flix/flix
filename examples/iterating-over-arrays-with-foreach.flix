// Easily iterate over arrays with foreach-loops.
def main(): Unit & Impure =
    use Array.iterator;

    region r { // Declare a new region.

        // Create two arrays in the new region r.
        let xs = [1, 2, 3, 4] @ r;
        let ys = [1, 2, 3, 4] @ r;

        // Create a nested foreach-loop
        // in a single expression.
        foreach (x <- iterator(r, xs);
                 y <- iterator(r, ys))
            println(x + y);

        // Equivalent to nesting like below.
        foreach (x <- iterator(r, xs))
            foreach(y <- iterator(r, ys))
                println(x + y)
    }
