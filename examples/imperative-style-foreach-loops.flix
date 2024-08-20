///
/// Flix supports writing code in an imperative
/// style if it is preferable to you.
///
/// One of the features related to this is a
/// foreach-loop.
///
/// In this example we explore how this loop
/// works in Flix.
///
def main(): Unit \ IO = region rc {

    //
    // Construct a simple list.
    //
    let l = 1 :: 2 :: 3 :: Nil;

    //
    // Here is a simple loop where we print the
    // elements of `l`.
    //
    // A loop starts with the `foreach` keyword
    // and has a pattern on the left side of
    // the arrow.
    // On the right side of the arrow we get
    // an iterator over `l`.
    //
    foreach (x <- l)
        println(x);

    //
    // Enumerator loops are not currently supported
    // in Flix. Supported for them will be revived
    // when `foreach` loops desugar to Streams
    // rather than Iterators.
    //
    // ~~~ Note that we can match the pattern that
    // the iterator on the right returns.
    //
    // In this case, the enumerator is an iterator
    // which also contains the index of the element.
    //
    // This may be familiar to you if you have
    // some experience with Rust. ~~~
    //
    // foreach ((x, i) <- List.enumerator(rc, l))
    //     println("${(x, i)}");

    //
    // foreach-loops are in fact just syntactic
    // sugar for `forEach` functions.
    //
    // Thus, by the type of `forEach` the last
    // expression in the foreach loop must be
    // of type unit.
    //
    let z = Ref.fresh(rc, Nil);
    foreach (x <- l)
        Ref.put(x :: Ref.get(z), z);

    //
    // The loop above is actually equivalent to
    // the function call below.
    //
    // However, foreach-loops can be much easier
    // to read than `foreach` function calls.
    //
    let q = Ref.fresh(rc, Nil);
    List.iterator(rc, l) |>
    Iterator.forEach(match x -> Ref.put(x :: Ref.get(q), q));

    //
    // This is true in particular for nested
    // foreach-loops.
    //
    let k = 4 :: 5 :: 6 :: Nil;
    let w = Ref.fresh(rc, Nil);
    foreach (a <- l) {
        Ref.put(a :: Ref.get(w), w);
        foreach (b <- k)
            Ref.put(b :: Ref.get(w), w)
    };

    //
    // The expression above is equivalent
    // to the functional style expression
    // below.
    //
    let v = Ref.fresh(rc, Nil);
    List.iterator(rc, l) |>
    Iterator.forEach(match a -> {
        Ref.put(a :: Ref.get(v), v);
        List.iterator(rc, k) |>
        Iterator.forEach(match b -> Ref.put(b :: Ref.get(v), v))
    });

    //
    // We can even simplify nested loops
    // further.
    //
    // We can have multiple loops separated by
    // a semicolon in a single parenthesis.
    //
    // This may be familiar to you if you have
    // some experience with Scala's for-loops.
    //
    let e = Ref.fresh(rc, Nil);
    foreach (a <- l;
             b <- k)
                Ref.put((a, b) :: Ref.get(e), e);

    //
    // The equivalent functional style expression
    // has a bit more visual noise.
    //
    let e1 = Ref.fresh(rc, Nil);
    List.iterator(rc, l) |>
    Iterator.forEach(match a -> {
        List.iterator(rc, k) |>
        Iterator.forEach(match b -> Ref.put((a, b) :: Ref.get(e1), e1))
    });

    //
    // Finally, we can also include guards
    // in our loop.
    //
    // With guards, you only gain access
    // to the values that satisfy the boolean
    // condition(s).
    //
    // These expressions are also separated by a
    // semicolon.
    //
    // Again, this is similar to Scala's
    // for-loops.
    //
    let c = Ref.fresh(rc, Nil);
    foreach (a <- l;
             if a > 1;
             b <- k;
             if b < 6)
                Ref.put((a, b) :: Ref.get(c), c);

    //
    // Alternatively, we can put a single guard
    // at the end of the parenthesis.
    //
    let d = Ref.fresh(rc, Nil);
    foreach (a <- l;
             b <- k;
             if a > 1 and b < 6)
                Ref.put((a, b) :: Ref.get(d), d);

    //
    // Here's the equivalent functional expression
    // which can be a bit cumbersome to read.
    //
    // **Summary*
    //
    // foreach-loops are great for working with
    // references and other imperative features,
    // where functional-style syntax may cumbersome
    // to read.
    // They are equivalent to function calls so
    // there is no difference in performance.
    // You can nest them, either explicitly or in
    // the same parenthesis, match on the exact pattern
    // and include guards.
    //
    let cf = Ref.fresh(rc, Nil);
    List.iterator(rc, l) |>
    Iterator.forEach(match a -> {
        List.iterator(rc, k) |>
        Iterator.forEach(match b -> {
            if (a > 1 and b < 6) Ref.put((a, b) :: Ref.get(cf), cf) else ()
        })
    });

    println("z == q? ${Ref.get(z) == Ref.get(q)}");
    println("w == l with k prepended to every element of l");
    println("w == ${Ref.get(w)}? ${Ref.get(w) == 6 :: 5 :: 4 :: 3 :: 6 :: 5 :: 4 :: 2 :: 6 :: 5 :: 4 :: 1 :: Nil}");
    println("w == v? ${Ref.get(w) == Ref.get(v)}");
    println("e contains all combinations of pairs from l and k");
    println("e (sorted) == ${List.sortBy(match (a, _) -> a, Ref.get(e))}");
    println("e == e1? ${Ref.get(e) == Ref.get(e1)}");
    println("c contains all combinations of pairs from l and k excluding 1 and 6");
    println("c (sorted) == ${List.sortBy(match (a, _) -> a, Ref.get(c))}");
    println("c == d? ${Ref.get(c) == Ref.get(d)}");
    println("c == cf? ${Ref.get(c) == Ref.get(cf)}")
}
