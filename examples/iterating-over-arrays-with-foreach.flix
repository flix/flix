// Easily iterate over arrays with foreach-loops.
def main(): Unit \ IO =
    use Array.iterator;

    region r { // Declare a new region.

        // Create two arrays in the new region r.
        let xs = [1, 2, 3, 4] @ r;
        let ys = [1, 2, 3, 4] @ r;

        // Create a nested foreach-loop
        // in a single expression.
        foreach (x <- xs;
                 y <- ys)
            println(x + y);

        // Equivalent to nesting like below.
        foreach (x <- xs)
            foreach(y <- ys)
                println(x + y)
    }
