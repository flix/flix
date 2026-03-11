// Easily iterate over lists with foreach-loops.
def main(): Unit \ IO =

    // Create two lists.
    let xs = 1 :: 2 :: 3 :: 4 :: Nil;
    let ys = 1 :: 2 :: 3 :: 4 :: Nil;

    // Create a nested foreach-loop
    // in a single expression.
    foreach (x <- xs;
             y <- ys)
        println(x + y);

    // Equivalent to nesting like below.
    foreach (x <- xs)
        foreach(y <- ys)
            println(x + y)
