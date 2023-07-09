mod Seq {

    pub restrictable enum Seq[s][a] {
        case Nil
        case One(a)
        case Cons(a, Seq[s][a])
    }

    type alias SOption[a] = Seq[<Seq.Nil, Seq.One>][a]
    type alias SList[a] = Seq[<Seq.Nil, Seq.Cons>][a]
    type alias SNel[a] = Seq[<Seq.One, Seq.Cons>][a]

    pub def main(): Unit \ IO  = {
        // Hovering over each of the declarations shows its inferred type.

        // We can create a sequence of one element
        let one = Seq.One(4);
        println("one: ${one}");

        // Mapping over the sequence does not change its type.
        // We know it is still exactly one element.
        let squared = map(x -> x * x, one);
        println("squared: ${squared}");

        // We can append s to itself. We know that the result is nonempty.
        let nonempty = append(one, one);
        println("nonempty: ${nonempty}");

        // If we a prefix of a nonempty list, we see that the result can involve any tags.
        let any = take(1, nonempty);
        println("any: ${any}");

        // But if we take a prefix of a singleton, we see that the result is either Nil or One.
        let option = take(1, one);
        println("option: ${option}");

        // We can take the head of a NonEmptyList
        let theHead = head(nonempty);
        println("theHead: ${theHead}");

        // But not of a list that may be empty (uncomment to see error)
        // let fail = head(option);

        // We can flatMap an Option to an Option.
        let a = option |> flatMap(x -> Seq.One(x));
        println("a: ${a}");

        // We can flatMap an Option to a List.
        let b = option |> flatMap(x -> open Seq.Cons(x, Seq.Nil));
        println("b: ${b}");

        // We can flatMap a NonEmptyList to a NonEmptyList
        let c = nonempty |> flatMap(x -> Seq.One(x));
        println("c: ${c}");
        ()
    }

    ///
    /// Renders the list `l` to a String.
    ///
    pub def toString(l: Seq[s][a]): String with ToString[a] = region rc {
        let sb = StringBuilder.new(rc);
        foreach (x <- l)
            StringBuilder.appendString!("${x} :: ", sb);
        StringBuilder.appendString!("Nil", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns true if and only if `l` is the empty list, i.e. `Nil`.
    ///
    pub def isEmpty(l: Seq[s][a]): Bool = choose l {
        case Seq.Nil    => true
        case Seq.One(_) => false
        case Seq.Cons(_, _) => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def head(l: Seq[s -- <Seq.Nil>][a]): a = choose l {
        case Seq.One(x)     => x
        case Seq.Cons(x, _) => x
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def last(l: Seq[s && <Seq.One, Seq.Cons>][a]): a = choose l {
        case Seq.One(x)         => x
        case Seq.Cons(_, xs)    => last(xs)
    }

    ///
    /// Returns the length of `l`.
    ///
    pub def length(l: Seq[s][a]): Int32 = choose l{
        case Seq.Nil => 0
        case Seq.One(_) => 1
        case Seq.Cons(_, xs) => length(xs) + 1
    }

    ///
    /// Returns `l2` appended to `l1`.
    ///
    ///
    pub def append(l1: Seq[s1][a], l2: Seq[s2][a]): Seq[s2 ++ <Seq.Cons>][a] = choose* l1 {
        case Seq.Nil            => l2
        case Seq.One(x)         => open Seq.Cons(x, l2)
        case Seq.Cons(x, xs)    => open Seq.Cons(x, append(xs, l2))
    }

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    pub def memberOf(a: a, l: Seq[s][a]): Bool with Eq[a] = choose l {
        case Seq.Nil            => false
        case Seq.One(x)         => a == x
        case Seq.Cons(x, xs)    => if (a == x) true else memberOf(a, xs)
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimum(l: Seq[s -- <Seq.Nil>][a]): a with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: Seq[s -- <Seq.Nil>][a]): a =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximum(l: Seq[s -- <Seq.Nil>][a]): a with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: Seq[s -- <Seq.Nil>][a]): a =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Optionally returns the position of `x` in `l`.
    ///
    pub def indexOf(a: a, l: Seq[s][a]): SOption[Int32] with Eq[a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => if (x == a) open Seq.One(0) else open Seq.Nil
        case Seq.Cons(x, xs)    => if (x == a) open Seq.One(0) else indexOf(a, xs) |> map(y -> y + 1)
    }

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, l: Seq[s][a]): SOption[a] \ ef = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, l: Seq[s][a]): SOption[a] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => if (f(x)) open Seq.One(x) else open Seq.Nil
        case Seq.Cons(x, xs)    => if (f(x)) open Seq.One(x) else findLeft(f, xs)
    }

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, l: Seq[s][a]): SOption[a] \ ef =
        findRightHelper(f, l, () -> open Seq.Nil)

    def findRightHelper(f: a -> Bool \ ef1, l: Seq[s][a], k: Unit -> SOption[a] \ ef2): SOption[a] \ {ef1, ef2} = choose* l {
        case Seq.Nil            => k()
        case Seq.One(x)         => if (f(x)) open Seq.One(x) else k()
        case Seq.Cons(x, xs)    => findRightHelper(f, xs, () -> if (f(x)) open Seq.One(x) else k())
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): SList[Int32] =
        if (b > e) {
            open Seq.Nil
        } else {
            open Seq.Cons(b, range(b + 1, e))
        }

    ///
    /// Returns a list with the element `x` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(n: Int32, a: a): SList[a] =
        if (n <= 0) {
            open Seq.Nil
        } else {
            open Seq.Cons(a, repeat(n - 1, a))
        }

    // ///
    // /// Alias for `scanLeft`.
    // ///
    // pub def scan(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef = scanLeft(f, s, l)

    ///
    /// Accumulates the result of applying `f` to `l` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, l: Seq[s][a]): SNel[b] \ ef = choose* l {
        case Seq.Nil            => Seq.One(s)
        case Seq.One(x)         => open Seq.Cons(s, Seq.One(f(s, x)))
        case Seq.Cons(x, xs)    => open Seq.Cons(s, scanLeft(f, f(s, x), xs))
    }

    // ///
    // /// Accumulates the result of applying `f` to `l` going right to left.
    // ///
    // /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    // ///
    pub def scanRight(f: (a, b) -> b \ ef, s: b, l: Seq[s][a]): SNel[b] \ ef = choose* l {
        case Seq.Nil            => Seq.One(s)
        case Seq.One(x)         => open Seq.Cons(f(x, s), Seq.One(s))
        case Seq.Cons(x, xs)    =>
            let r = scanRight(f, s, xs);
            open Seq.Cons(f(x, head(r)), r)
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, l: Seq[s][a]): Seq[s][b] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => Seq.One(f(x))
        case Seq.Cons(x, xs)    => open Seq.Cons(f(x), map(f, xs))
    }

    ///
    /// Return the singleton list with element `x`.
    ///
    pub def point(a: a) : Seq[<Seq.One>][a] = Seq.One(a)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: Seq[s][a]): Seq[s][b] \ ef = {
        l |> zipWithIndex |> map(match (i, x) -> f(i, x))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    pub def flatMap(f: a -> Seq[s1][b] \ ef, l: Seq[s2][a]): Seq[(s2 && <Seq.Nil, Seq.Cons>) ++ s1][b] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => f(x)
        case Seq.Cons(x, xs)    => append(f(x), flatMap(f, xs))
    }

    ///
    /// Returns the reverse of `l`.
    ///
    pub def reverse(l: Seq[s][a]): Seq[(s && <Seq.Nil>) ++ <Seq.One, Seq.Cons>][a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => Seq.One(x)
        case Seq.Cons(x, xs)    => revAppend(xs, Seq.One(x))
    }

    def revAppend(l1: Seq[s1][a], l2: Seq[s2][a]): Seq[s2 ++ <Seq.Cons>][a] = choose* l1 {
        case Seq.Nil            => l2
        case Seq.One(x)         => open Seq.Cons(x, l2)
        case Seq.Cons(x, xs)    => revAppend(xs, open Seq.Cons(x, l2))
    }

    ///
    /// Returns `l` with the element at index `i` replaced by `x`.
    ///
    /// Returns `l` if `i < 0` or `i > length(l)-1`.
    ///
    pub def update(i: Int32, a: a, l: Seq[s][a]): Seq[s][a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => if (i == 0) Seq.One(a) else Seq.One(x)
        case Seq.Cons(x, xs)    => if (i == 0) open Seq.Cons(a, xs) else open Seq.Cons(x, update(i - 1, a, xs))
    }

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    pub def replace(from: {from = a}, to: {to = a}, l: Seq[s][a]): Seq[s][a] with Eq[a] = map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, l: Seq[s][a]): Seq[s ++ <Seq.Cons>][a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => Seq.One(x)
        case Seq.Cons(x, xs)    => open Seq.Cons(x, presperse(a, xs))
    }

    def presperse(a: a, l: Seq[s][a]): Seq[s ++ <Seq.Cons>][a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => open Seq.Cons(a, Seq.One(x))
        case Seq.Cons(x, xs)    => open Seq.Cons(a, open Seq.Cons(x, presperse(a, xs)))
    }

    ///
    /// Returns the concatenation of the elements in `l2` with the elements of `l1` inserted between every two adjacent elements.
    ///
    /// That is, returns `y1 :: x1 ... xn :: y2 :: ... yn-1 :: x1 :: ... :: xn :: yn :: Nil`.
    ///
    pub def intercalate(l1: Seq[s1][a], l2: Seq[s2o][Seq[s2i][a]]): Seq[(s2o && <Seq.Nil>) ++ s2i ++ <Seq.Cons>][a] = choose* l2 {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => x
        case Seq.Cons(x, xs)    => append(x, precalate(l1, xs))
    }

    def precalate(l1: Seq[s1][a], l2: Seq[s2o][Seq[s2i][a]]): Seq[(s2o && <Seq.Nil>) ++ s2i ++ <Seq.Cons>][a] = choose* l2 {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => append(l1, x)
        case Seq.Cons(x, xs)    => append(append(l1, x), precalate(l1, xs))
    }

    ///
    /// Returns `true` if and only if `l1` is a prefix of `l2`.
    ///
    pub def isPrefixOf(l1: Seq[s1][a], l2: Seq[s2][a]): Bool with Eq[a] = choose l1 {
        case Seq.Nil            => true
        case Seq.One(x)         =>
            choose l2 {
                case Seq.Nil        => false
                case Seq.One(y)     => x == y
                case Seq.Cons(y, _) => x == y
            }
        case Seq.Cons(x, xs)    =>
            choose l2 {
                case Seq.Nil            => false
                case Seq.One(y)         => x == y
                case Seq.Cons(y, ys)    => x == y and xs `isPrefixOf` ys
            }
    }

    ///
    /// Returns `true` if and only if `l1` is an infix of `l2`.
    ///
    pub def isInfixOf(l1: Seq[s1][a], l2: Seq[s2][a]): Bool with Eq[a] = choose l1 {
        case Seq.Nil            => true
        case Seq.One(x)         => memberOf(x, l2)
        case Seq.Cons(x, xs)    =>
            choose l2 {
                case Seq.Nil            => false
                case Seq.One(y)         => x == y and isEmpty(xs)
                case Seq.Cons(y, ys)    => (x == y and xs `isPrefixOf` ys) or l1 `isInfixOf` ys
            }
    }

    ///
    /// Returns `true` if and only if `l1` is a suffix of `l2`.
    ///
    pub def isSuffixOf(l1: Seq[s1][a], l2: Seq[s2][a]): Bool with Eq[a] = isPrefixOf(reverse(l1), reverse(l2))

    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: List[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: Seq[s][a]): b \ ef = choose l {
        case Seq.Nil            => s
        case Seq.One(x)         => f(s, x)
        case Seq.Cons(x, xs)    => foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: Seq[s][a]): b \ ef = choose l {
        case Seq.Nil            => s
        case Seq.One(x)         => f(x, s)
        case Seq.Cons(x, xs)    => f(x, foldRight(f, s, xs))
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: Seq[s][a]): b \ ef = choose l {
        case Seq.Nil            => z
        case Seq.One(x)         => f(x, _ -> checked_ecast(z))
        case Seq.Cons(x, xs)    => f(x, _ -> foldRightWithCont(f, z, xs))
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, l: Seq[s -- <Seq.Nil>][a]): a \ ef = choose l {
        case Seq.One(x)         => x
        case Seq.Cons(x, xs)    => foldLeft(f, x, xs)
    }

    ///
    /// Applies `f` to all elements in `l` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, l: Seq[s -- <Seq.Nil>][a]): a \ ef = choose l {
        case Seq.One(x)         => x
        case Seq.Cons(x, xs)    => foldRight(f, x, xs)
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, l: Seq[s][a]): Int32 \ ef =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    pub def flatten(l: Seq[s1][Seq[s2][a]]): Seq[s2 ++ (s1 && <Seq.Nil, Seq.Cons>)][a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => x
        case Seq.Cons(x, xs)    => append(x, flatten(xs))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, l: Seq[s][a]): Bool \ ef = choose l {
        case Seq.Nil            => false
        case Seq.One(x)         => f(x)
        case Seq.Cons(x, xs)    => f(x) or exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, l: Seq[s][a]): Bool \ ef = choose l {
        case Seq.Nil            => true
        case Seq.One(x)         => f(x)
        case Seq.Cons(x, xs)    => f(x) and forAll(f, xs)
    }

    ///
    /// Returns a list of every element in `l` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, l: Seq[s][a]): Seq[s ++ <Seq.Nil>][a] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => if (f(x)) open Seq.One(x) else open Seq.Nil
        case Seq.Cons(x, xs)    => if (f(x)) open Seq.Cons(x, filter(f, xs)) else open_as Seq filter(f, xs)
    }

    ///
    /// Returns the sublist of `l` without the first element.
    /// Returns `None` if the list `l` is `Nil`.
    ///
    pub def tail(l: Seq[s -- <Seq.Nil>][a]): Seq[s ++ <Seq.Nil>][a] = choose* l {
        case Seq.One(_)         => Seq.Nil
        case Seq.Cons(_, xs)    => xs
    }

    ///
    /// Returns the sublist of `l` from index `start` (inclusive) to index `end` (exclusive).
    ///
    /// That is, an element at index `i` in `l` is part of the returned sublist if and only if `i >= start` and `i < end`.
    /// Note: Indices that are out of bounds in `l` are not considered (i.e. slice(start, end, l) = slice(max(0, start), min(length(l), end), l)).
    ///
    pub def slice(start: {start = Int32}, end: {end = Int32}, l: Seq[s][a]): Seq[s ++ <Seq.Nil>][a] =
        l |> drop(start.start) |> take(end.end - start.start)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(l)`.
    /// Returns `l` if `n < 0`.
    ///
    pub def drop(n: Int32, l: Seq[s][a]): Seq[s ++ <Seq.Nil>][a] = {
        if (n <= 0) {
            open_as Seq l
        } else {
            choose* l {
                case Seq.Nil            => Seq.Nil
                case Seq.One(_)         => Seq.Nil
                case Seq.Cons(_, xs)    => drop(n - 1, xs)
            }
        }
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, l: Seq[s][a]): Seq[s ++ <Seq.Nil>][a] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => if (f(x)) open Seq.Nil else open Seq.One(x)
        case Seq.Cons(x, xs)    => if (f(x)) open_as Seq dropWhile(f, xs) else open Seq.Cons(x, open_as Seq xs)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Returns `l` if `n > length(l)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def take(n: Int32, l: Seq[s][a]): Seq[s ++ <Seq.Nil>][a] = {
        if (n <= 0) {
            open Seq.Nil
        } else {
            choose* l {
                case Seq.Nil            => Seq.Nil
                case Seq.One(x)         => Seq.One(x)
                case Seq.Cons(x, xs)    => open Seq.Cons(x, take(n - 1, xs))
            }
        }
    }

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, l: Seq[s][a]): Seq[s ++ <Seq.Nil>][a] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => if (f(x)) open Seq.One(x) else open Seq.Nil
        case Seq.Cons(x, xs)    => if (f(x)) open Seq.Cons(x, takeWhile(f, xs)) else open_as Seq takeWhile(f, xs)
    }

    ///
    /// Split the list `xs` at the position `n` returning the left and right parts.
    /// Position `n` is included in the right part.
    ///
    /// Example: `splitAt(2, 1::2::3::4::Nil)` returns `(1::2::Nil, 3::4::Nil)`
    ///
    /// Returns `(xs, Nil)` if `n > length(xs)`.
    /// Returns `(Nil, xs)` if `n < 0`.
    ///
    pub def splitAt(n: Int32, xs: Seq[s][a]): (Seq[s ++ <Seq.Nil>][a], Seq[s ++ <Seq.Nil>][a]) =
        (take(n, xs), drop(n, xs))

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zip(l1: Seq[s1][a], l2: Seq[s2][b]): Seq[((s1 ++ s2) && <Seq.Nil>) ++ ((s1 ++ s2) && <Seq.One>) ++ ((s1 && s2) && <Seq.Cons>)][(a, b)] = choose* l1 {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         =>
            choose* l2 {
                case Seq.Nil            => Seq.Nil
                case Seq.One(y)         => Seq.One((x, y))
                case Seq.Cons(y, _)     => Seq.One((x, y))
            }
        case Seq.Cons(x, xs)    =>
            choose* l2 {
                case Seq.Nil            => Seq.Nil
                case Seq.One(y)         => Seq.One((x, y))
                case Seq.Cons(y, ys)    => open Seq.Cons((x, y), zip(xs, ys))
            }
    }

    // ///
    // /// Returns a list where the element at index `i` is `f(a, b)` where
    // /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    // ///
    // /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    // ///
    pub def zipWith(f: (a, b) -> c \ ef, l1: Seq[s1][a], l2: Seq[s2][b]): Seq[((s1 ++ s2) && <Seq.Nil>) ++ ((s1 ++ s2) && <Seq.One>) ++ ((s1 && s2) && <Seq.Cons>)][c] \ ef = {
        zip(l1, l2) |> map(match (x, y) -> f(x, y))
    }

    ///
    /// Returns a list where each element `e` is mapped to `(i, e)` where `i`
    /// is the index of `e`.
    ///
    pub def zipWithIndex(l: Seq[s][a]): Seq[s][(Int32, a)] = zipWithIndexHelper(0, l)

    def zipWithIndexHelper(i: Int32, l: Seq[s][a]): Seq[s][(Int32, a)] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => Seq.One((i, x))
        case Seq.Cons(x, xs)    => open Seq.Cons((i, x), zipWithIndexHelper(i + 1, xs))
    }

    ///
    /// Returns a pair of lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    pub def unzip(l: Seq[s][(a, b)]): (Seq[s][a], Seq[s][b]) = {
        (map(fst, l), map(snd, l))
    }

    // ///
    // /// Returns a list where the element at index `i` is `(a, b, c)` where
    // /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    // /// and `c` is the element at index `i` in `l3`.
    // ///
    // /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    // ///
    pub def zip3(l1: Seq[s1][a], l2: Seq[s2][b], l3: Seq[s3][c]): Seq[((s1 ++ s2 ++ s3) && <Seq.Nil>) ++ ((s1 ++ s2 ++ s3) && <Seq.One>) ++ ((s1 && s2 && s3) && <Seq.Cons>)][(a, b, c)] = {
        (l1 `zip` l2 `zip` l3) |> map(match ((x, y), z) -> (x, y, z))
    }

    ///
    /// Returns a list where the element at index `i` is `f(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith3(f: (a, b, c) -> d \ ef, l1: Seq[s1][a], l2: Seq[s2][b], l3: Seq[s3][c]): Seq[((s1 ++ s2 ++ s3) && <Seq.Nil>) ++ ((s1 ++ s2 ++ s3) && <Seq.One>) ++ ((s1 && s2 && s3) && <Seq.Cons>)][d] \ ef = {
        zip3(l1, l2, l3) |> map(match (x, y, z) -> f(x, y, z))
    }

    ///
    /// Returns a triple of lists, the first containing all first components in `l`
    /// the second containing all second components in `l` and the third containing all
    /// third components in `l`.
    ///
    pub def unzip3(l: Seq[s][(a, b, c)]): (Seq[s][a], Seq[s][b], Seq[s][c]) = {
        let l1 = l |> map(match (x, _, _) -> x);
        let l2 = l |> map(match (_, y, _) -> y);
        let l3 = l |> map(match (_, _, z) -> z);
        (l1, l2, l3)
    }

    ///
    /// Alias for `foldLeft2`.
    ///
    pub def fold2(f: (c, a, b) -> c \ ef, c: c, l1: Seq[s1][a], l2: Seq[s2][b]): c \ ef = foldLeft2(f, c, l1, l2)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from left to right.
    ///
    pub def foldLeft2(f: (c, a, b) -> c \ ef, c: c, l1: Seq[s1][a], l2: Seq[s2][b]): c \ ef = {
        (c, zip(l1, l2)) ||> foldLeft(x -> match (y, z) -> f(x, y, z))
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from right to left.
    ///
    pub def foldRight2(f: (a, b, c) -> c \ ef, c: c, l1: Seq[s1][a], l2: Seq[s2][b]): c \ ef =
        (c, zip(l1, l2)) ||> foldRight(match (x, y) -> z -> f(x, y, z))


    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: Seq[s][a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element of `l` is `None`.
    ///
    pub def findMap(f: a -> Seq[s1 && <Seq.Nil, Seq.One>][b] \ ef, l: Seq[s2][a]): Seq[(s2 && <Seq.Nil>) ++ (s1 && <Seq.Nil, Seq.One>)][b] \ ef = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => f(x)
        case Seq.Cons(x, xs)    =>
            choose* f(x) {
                case Seq.Nil    => findMap(f, xs)
                case Seq.One(v) => Seq.One(v)
            }
    }

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` with `sep` inserted between each element.
    ///
    pub def join(sep: String, l: Seq[s][a]): String with ToString[a] =
        Foldable.join(sep, l)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, l: Seq[s][a]): String \ ef =
        Foldable.joinWith(f, sep, l)

    ///
    /// Returns the elements of `l` as a `DelayList`.
    ///
    pub def toDelayList(l: Seq[s][a]): DelayList[a] = choose l {
        case Seq.Nil            => DelayList.ENil
        case Seq.One(x)         => DelayList.LCons(x, lazy DelayList.ENil)
        case Seq.Cons(x, xs)    => DelayList.LCons(x, lazy toDelayList(xs))
    }

    ///
    /// Returns the list `l` as a chain.
    ///
    pub def toChain(l: Seq[s][a]): Chain[a] =
        foldLeft(Chain.snoc, Chain.empty(), l)

    ///
    /// Returns `l` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], l: Seq[s][a]): MutDeque[a, r] \ r =
        let d = MutDeque.new(rc);
        forEach(x -> MutDeque.pushBack(x, d), l);
        d

    ///
    /// Returns `l` as a mutable list.
    ///
    pub def toMutList(rc: Region[r], l: Seq[s][a]): MutList[a, r] \ r = region rc2 {
        Array.toMutList(rc, toArray(rc2, l)) // `Array.toMutList` respects the invariant of `MutList`
    }

    ///
    /// Returns the list `l` as a set.
    ///
    pub def toSet(l: Seq[s][a]): Set[a] with Order[a] =
        foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), l)

    ///
    /// Returns the association list `l` as a map.
    ///
    /// If `l` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(l: Seq[s][(a, b)]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> Map.insert(fst(x), snd(x), acc), Map.empty(), l)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    /// If `s` contains multiple mappings with the same key, `toMapWith` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMapWith(f: a -> b, l: Seq[s][a]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    pub def forEach(f: a -> Unit \ ef, l: Seq[s][a]): Unit \ ef = choose l {
        case Seq.Nil            => ()
        case Seq.One(x)         => f(x)
        case Seq.Cons(x, xs)    => f(x); forEach(f, xs)
    }

    ///
    /// Applies `f` to every element of `l` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, l: Seq[s][a]): Unit \ ef = {
        l |> zipWithIndex |> forEach(match (i, x) -> f(i, x))
    }

    ///
    /// Returns the list `l` as an array.
    ///
    pub def toArray(rc: Region[r], l: Seq[s][a]): Array[a, r] \ r = {
            let a = Array.new(rc, length(l));
            forEach(match (i, b) -> Array.put(b, i, a), zipWithIndex(l));
            a
        }

    ///
    /// Returns the list `l` as a vector.
    ///
    pub def toVector(l: Seq[s][a]): Vector[a] = region rc {
        let arr = Array.new(rc, length(l));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), l);
        Array.toVector(arr)
    }

    // ///
    // /// Sort list `l` so that elements are ordered from low to high according to their `Order` instance.
    // ///
    // /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    // ///
    pub def sort(l: Seq[s][a]): Seq[s][a] with Order[a] = choose* l {
        case Seq.Nil            => Seq.Nil
        case Seq.One(x)         => Seq.One(x)
        case Seq.Cons(x, xs)    => insert(x, sort(xs))
    }

    def insert(x: a, l: Seq[s][a]): Seq[s ++ <Seq.Cons>][a] with Order[a] = choose* l {
        case Seq.Nil            => open Seq.Cons(x, Seq.Nil)
        case Seq.One(y)         => if (x <= y) open Seq.Cons(x, Seq.One(y)) else open Seq.Cons(y, Seq.One(x))
        case Seq.Cons(y, ys)    => if (x <= y) open Seq.Cons(x, open Seq.Cons(y, ys)) else open Seq.Cons(y, insert(x, ys))
    }

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: Seq[s][Int32]): Int32 =
        Foldable.sum(l)

    ///
    /// Returns the sum of all elements in the list `l` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, l: Seq[s][a]): Int32 \ ef =
        Foldable.sumWith(f, l)

    ///
    /// Returns an iterator over `l`.
    ///
    pub def iterator(rc: Region[r], xs: Seq[s][a]): Iterator[a, r, r] \ r =
        let ls = ref open_as Seq xs @ rc;
        let next = () -> {
            choose (deref ls) {
                case Seq.Nil            => None
                case Seq.One(x)         => ls := open Seq.Nil; Some(x)
                case Seq.Cons(x, rs)    => ls := rs; Some(x)
            }
        };
        Iterator.iterate(rc, next)

    //
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], l: Seq[s][a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, l) |> Iterator.zipWithIndex

    ///
    /// Returns the association list `l` as a `DelayMap`.
    ///
    /// If `l` contains multiple mappings with the same key, `toDelayMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toDelayMap(l: Seq[s][(a, b)]): DelayMap[a, b] with Order[a] =
        foldRight((x, acc) -> DelayMap.insert(fst(x), snd(x), acc), DelayMap.empty(), l)


}


instance ToString[Seq.Seq[s][a]] with ToString[a] {
    pub def toString(l: Seq.Seq[s][a]): String = Seq.toString(l)
}

instance Hash[Seq.Seq[s][a]] with Hash[a] {
    pub def hash(l: Seq.Seq[s][a]): Int32 =
        Seq.foldLeft((acc, x) -> acc `Hash.combine` Hash.hash(x), Hash.magic(), l)
}

instance Eq[Seq.Seq[s][a]] with Eq[a] {
    pub def eq(l1: Seq.Seq[s][a], l2: Seq.Seq[s][a]): Bool = l1 `Seq.isPrefixOf` l2 and l2 `Seq.isPrefixOf` l1
}

instance Order[Seq.Seq[s][a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: Seq.Seq[s][a], l2: Seq.Seq[s][a]): Comparison = {
        let l = Seq.zip(l1, l2);
        compareHelper(l) `Order.thenCompare` lazy (Seq.length(l1) <=> Seq.length(l2))
    }

}

def compareHelper(l: Seq.Seq[s][(a, a)]): Comparison with Order[a] = choose l {
    case Seq.Seq.Nil            => Comparison.EqualTo
    case Seq.Seq.One(x)         => fst(x) <=> snd(x)
    case Seq.Seq.Cons(x, xs)    => (fst(x) <=> snd(x)) `Order.thenCompare` lazy compareHelper(xs)
}

instance Functor[Seq.Seq[s]] {
    pub def map(f: a -> b \ ef, l: Seq.Seq[s][a]): Seq.Seq[s][b] \ ef = Seq.map(f, l)
}

instance Foldable[Seq.Seq[s]] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: Seq.Seq[s][a]): b \ ef = Seq.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: Seq.Seq[s][a]): b \ ef = Seq.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, l: Seq.Seq[s][a]): b \ ef = Seq.foldRightWithCont(f, s, l)
}

instance Iterable[Seq.Seq[s]] {
    pub def iterator(rc: Region[r], l: Seq.Seq[s][a]): Iterator[a, r, r] \ r = Seq.iterator(rc, l)
}
